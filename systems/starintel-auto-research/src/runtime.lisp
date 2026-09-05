;;;; runtime.lisp --- the Auto Research application runtime.
;;;;
;;;; Implements the core protocols for this adapter set. API, CLI, cron,
;;;; and agents call THESE operations. Forgejo-originated requests and
;;;; API-originated requests converge here and are deduplicated by the
;;;; qualified issue identity. ARADR semantics are enforced through
;;;; starintel-aradr; semantic work goes through the cl-prolog-rlm
;;;; harness in :direct mode.

(in-package #:starintel-auto-research)

(defclass ar-runtime ()
  ((root :initarg :root :reader ar-root)
   (store :initarg :store :reader ar-store)
   (forgejo :initarg :forgejo :initform nil :reader ar-forgejo)
   (rlm-registry :initarg :rlm-registry :reader ar-rlm-registry)
   (run-ledger :initarg :run-ledger :reader ar-run-ledger)
   (candidate-ledger :initarg :candidate-ledger :reader ar-candidate-ledger)
   (request-ledger :initarg :request-ledger :reader ar-request-ledger)
   (event-store :initarg :event-store :reader ar-event-store)
   (operator :initarg :operator :reader ar-operator)
   (provider :initarg :provider :initform nil :reader ar-provider)
    (runs :initform (make-hash-table :test #'equal) :reader ar-runs-table)
    (requests :initform (make-hash-table :test #'equal) :reader ar-requests-table)
    (candidates :initform (make-hash-table :test #'equal) :reader ar-candidates)
    (revision-counts :initform (make-hash-table :test #'equal)
                     :reader ar-revision-counts)
    (run-artifacts :initform (make-hash-table :test #'equal)
                   :reader ar-run-artifacts-slot)
    (run-evidence :initform (make-hash-table :test #'equal)
                  :reader ar-run-evidence-slot)))

(defun make-ar-runtime (root &key forgejo rlm-registry provider
                                  (operator "auto-research-runtime"))
  "Build a runtime over the canonical corpus at ROOT. ROOT is the
authoritative Auto Research checkout; it is never cloned or substituted.
The semantic tool registry is installed here so every run receives exactly
the bounded semantic tool set."
  (let* ((store (make-org-roam-store root))
         (roam (ensure-roam-layout root))
         (runtime (make-instance
                   'ar-runtime
                   :root (namestring root)
                   :store store
                   :forgejo forgejo
                   :rlm-registry (or rlm-registry (make-research-tool-registry))
                   :provider provider
                   :run-ledger (make-run-ledger roam)
                   :candidate-ledger (make-candidate-ledger roam)
                   :event-store (research-core:make-jsonl-event-store
                                 (merge-pathnames ".ar-events.jsonl" root))
                   :operator operator)))
    ;; the request ledger records processed issue idempotency keys
    (setf (slot-value runtime 'request-ledger)
          (make-instance 'ledger
                         :path (merge-pathnames ".research-requests" root)))
    (install-research-tools (ar-rlm-registry runtime) runtime)
    runtime))

;;; --------------------------------------------------------------- config

(defun forgejo-repository-of (project)
  (getf (research-repository-forgejo (project-config-research-repository project))
        :repository))

(defun forgejo-instance-of (project)
  (getf (research-repository-forgejo (project-config-research-repository project))
        :base-url))

;;; --------------------------------------------------------------- events

(defun ar-record-event (runtime type entity-id &key previous-state next-state
                                              actor payload evidence)
  (research-core:append-event
   (ar-event-store runtime)
   (research-core:make-event type entity-id
                             :previous-state previous-state
                             :next-state next-state
                             :actor (or actor "auto-research-runtime")
                             :payload payload
                             :evidence evidence)))

;;; -------------------------------------------------------- request plane

(defun find-request-by-key (runtime key)
  "Resolve an already-processed qualified issue identity to its request id."
  (block scan
    (unless (probe-file (ledger-path (ar-request-ledger runtime)))
      (return-from scan nil))
    (with-open-file (in (ledger-path (ar-request-ledger runtime))
                        :direction :input :external-format :utf-8)
      (loop
        :for line := (read-line in nil nil)
        :while line
        :unless (str:blankp line)
          :do (let ((table (rc.j:to-table (rc.j:parse-value line))))
                (when (string= (gethash "entity_id" table "") key)
                  (return-from scan (gethash "next_state" table))))))))

(defun record-request-key (runtime key request-id)
  (ledger-append (ar-request-ledger runtime)
                 (sar.i:dict "event_id" (sar.i:short-id "req")
                             "event_type" "forgejo.issue.linked"
                             "entity_id" key
                             "next_state" request-id
                             "timestamp" (sar.i:now-iso8601))))

(defun forgejo-ensure-issue (runtime project question)
  "API-origin request: create the Forgejo issue once. Idempotent by
deterministic issue title. Returns (values issue-number created-p)."
  (let ((client (ar-forgejo runtime)))
    (when client
      (let* ((title (research-issue-title project question))
             (existing (find-forgejo-issue-by-title
                        client (forgejo-repository-of project) title)))
        (if existing
            (values (gethash "number" existing) nil)
            (let* ((response (forgejo-create-issue
                              client (forgejo-repository-of project)
                              title
                              (research-issue-body project question)))
                   (issue (rc.j:to-table (rc.j:parse-value response))))
              (values (gethash "number" issue) t)))))))

(defun research-issue-title (project question)
  (format nil "[research] ~a: ~a"
          (project-config-id project)
          (subseq question 0 (min 80 (length question)))))

(defun research-issue-body (project question)
  (format nil "<!-- starintel-research-request:v1 -->~%Project: ~a~%Question: ~a~%"
          (project-config-id project) question))

(defun find-forgejo-issue-by-title (client repository title)
  (ignore-errors
   (let ((issues (rc.j:parse-value
                  (forgejo-list-issues client repository))))
     (dolist (issue issues)
       (let ((table (rc.j:to-table issue)))
         (when (string= (gethash "title" table "") title)
           (return table)))))))

;;; -------------------------------------------------- submission (API path)

(defun ar-submit-research (runtime project question &key (source :api) issue-ref
                                                     input-kind input-path)
  "API-origin request: canonical request -> issue in the configured
research repo -> ARDR issue transaction -> run. Returns
(values request run issue-number existing-issue-p). INPUT-KIND and
INPUT-PATH record the resolved invocation shape (file | text and, when
applicable, the file) as request provenance; the project identity is
the explicitly resolved PROJECT and is immutable afterwards."
  (multiple-value-bind (issue-number created-p)
      (forgejo-ensure-issue runtime project question)
    (let* ((request-id (sar.i:short-id "req"))
           (run-id (sar.i:short-id "run"))
           (request (make-instance 'research-core:research-request
                                   :id request-id
                                   :project project
                                   :question question
                                   :source source
                                   :issue-ref issue-ref
                                   :input-kind input-kind
                                   :input-path input-path
                                   :status :received
                                   :run-ids (list run-id)
                                   :created-at (sar.i:now-iso8601)))
           (run (make-instance 'research-core:research-run
                               :id run-id
                               :request-id request-id
                               :project project
                               :status :queued
                               :budget (find-budget-profile
                                        (project-config-budget-name project))
                               :created-at (sar.i:now-iso8601))))
      (ar-record-event runtime :request.received request-id
                       :next-state :received
                       :payload (sar.i:dict "question" question
                                            "source" (string-downcase (symbol-name source))
                                            "project" (project-config-id project)
                                            "input_kind" (and input-kind (string-downcase (symbol-name input-kind)))
                                            "input_path" input-path))
      (ar-record-event runtime :run.created run-id :next-state :queued
                       :payload (sar.i:dict "request_id" request-id))
      (when issue-number
        (write-ardr-issue-transaction
         (ar-store runtime)
         (forgejo-instance-of project)
         (forgejo-repository-of project)
         issue-number
         (project-config-roam-slug project)
         (format nil "ARDR issue ~a#~a" (forgejo-repository-of project) issue-number)
         question)
        (ar-record-event runtime :forgejo.issue.created
                         (format nil "~a#~a" (forgejo-repository-of project) issue-number)
                         :payload (sar.i:dict "created" (not created-p)))
        ;; claim the qualified identity so the webhook round-trip dedups
        (record-request-key
         runtime
         (forgejo-issue-ref (forgejo-instance-of project)
                            (forgejo-repository-of project)
                            issue-number)
         request-id))
      (setf (gethash request-id (ar-requests-slot runtime)) request)
      (setf (gethash run-id (ar-runs-slot runtime)) run)
      (values request run issue-number (not created-p)))))

;;; ---------------------------------------------------------- registries

(defun ar-runs-slot (runtime)
  (slot-value runtime 'runs))

(defun ar-requests-slot (runtime)
  (slot-value runtime 'requests))

(defun ar-get-run (runtime run-id)
  (gethash run-id (ar-runs-slot runtime)))

(defun ar-get-request (runtime request-id)
  (gethash request-id (ar-requests-slot runtime)))

(defun ar-list-runs (runtime &optional status)
  (let ((runs))
    (loop :for run :being :the :hash-values :of (ar-runs-slot runtime)
          :do (when (or (null status) (eq (research-core:run-status run) status))
                (push run runs)))
    (sort runs #'string< :key #'research-core:run-id)))

;;; -------------------------------------------------- Forgejo-origin path

(defmethod research-core:process-forgejo-event ((runtime ar-runtime) event)
  "Forgejo-origin request. Resolves the repository to a project, looks up
the qualified issue identity, and creates request/transaction only if
absent. Returns (values result created-p)."
  (multiple-value-bind (instance repository number action title body)
      (forgejo-issue-from-payload (if (hash-table-p event)
                                      event
                                      (rc.j:to-table event)))
    (unless (and instance repository number)
      (error 'research-core:validation-error
             :message "webhook payload lacks qualified issue identity"))
    (let* ((key (forgejo-issue-ref instance repository number))
           (config (find-project-config-by-forgejo-repo runtime repository)))
      (unless config
        (error 'research-core:validation-error
               :message (format nil "no project configured for repository ~a" repository)))
      (let ((existing (find-request-by-key runtime key)))
        (if existing
            (values (list :existing existing) nil)
            (multiple-value-bind (request run issue-number existing-issue-p)
                (ar-submit-research
                 runtime config
                 (or (extract-question body)
                     (and (stringp title) title)
                     "No question recorded.")
                 :source :forgejo
                 :issue-ref (research-core:make-issue-ref
                             :forgejo instance repository number))
              (declare (ignore run existing-issue-p))
              ;; the transaction node uses the PAYLOAD identity: the issue
              ;; already exists on the Forgejo side
              (when (and instance repository number)
                (write-ardr-issue-transaction
                 (ar-store runtime) instance repository number
                 (project-config-roam-slug config)
                 (format nil "ARDR issue ~a#~a" repository number)
                 (research-core:request-question request)))
              ;; record the qualified identity so the API-created issue
              ;; returning through the webhook resolves to this request
              (record-request-key runtime key (research-core:request-id request))
              (values (list :created key) t)))))))

(defun find-project-config-by-forgejo-repo (runtime repository)
  (loop :for id :being :the :hash-keys :of (project-configs)
          :using (hash-value config)
        :when (and (forgejo-repository-of config)
                   (string= (forgejo-repository-of config) repository))
          :return config))

(defun extract-question (body)
  "Extract the bounded question from an issue body: the line after
`Question: ' if present."
  (when (stringp body)
    (let ((match (ppcre:scan-to-strings "(?im)^Question:[ \\t]*(.+)$" body)))
      (and match (string-trim " " (aref match 0))))))

;;; ---------------------------------------------------- candidate handling

(defun ar-submit-candidate (runtime run-id problem hypothesis predicted-result
                             falsifier &optional parent-ids
                             &key evidence acceptance-criteria rejected-alternatives)
  "Create one bounded candidate per the approved contract and record the
candidate.created event at :queued. EVIDENCE is the primary citation for
the central claim, ACCEPTANCE-CRITERIA the observable completion test, and
REJECTED-ALTERNATIVES one considered-and-rejected alternative; all three
are recorded as structured lists for the deterministic vetoes."
  (let ((candidate-id (sar.i:short-id "cand"))
        (parents (or parent-ids nil)))
    (unless (<= 0 (length parents) 2)
      (error 'aradr:deterministic-veto :rule :invalid-parents
             :detail "candidates record zero, one, or two parents"))
    (setf (gethash candidate-id (ar-candidates runtime))
          (list (cons :id candidate-id)
                (cons :run-id run-id)
                (cons :parent-ids parents)
                (cons :state :queued)
                (cons :problem problem)
                (cons :hypothesis hypothesis)
                (cons :predicted-result predicted-result)
                (cons :falsifier falsifier)
                (cons :evidence-for (when evidence (list evidence)))
                (cons :acceptance-criteria
                      (when acceptance-criteria (list acceptance-criteria)))
                (cons :rejected-alternatives
                      (when rejected-alternatives (list rejected-alternatives)))
                (cons :evidence-against nil)
                (cons :reviews nil)
                (cons :created-at (sar.i:now-iso8601))))
    (ledger-append (ar-candidate-ledger runtime)
                   (ledger-record :candidate.created candidate-id
                                  :next-state :queued
                                  :payload (sar.i:dict
                                            "hypothesis" hypothesis
                                            "problem" problem)))
    candidate-id))

(defun ar-candidate-transition (runtime candidate-id to &key reason)
  "Move one candidate through its approved lifecycle; invalid transitions
fail closed."
  (let ((candidate (or (gethash candidate-id (ar-candidates runtime))
                       (error 'research-core:validation-error
                              :message (format nil "unknown candidate: ~a" candidate-id)))))
    (let ((from (cdr (assoc :state candidate))))
      (when (and (eq from :evaluated) (eq to :proposing))
        ;; EVALUATED -> PROPOSING only for ONE bounded aggregator revision
        (when (>= (gethash candidate-id (ar-revision-counts runtime) 0) 1)
          (error 'aradr:transition-error :from from :to to))
        (incf (gethash candidate-id (ar-revision-counts runtime) 0)))
      (setf (cdr (assoc :state candidate))
            (aradr:candidate-transition from to :reason reason))
      (ledger-append (ar-candidate-ledger runtime)
                     (ledger-record :candidate.state-changed candidate-id
                                    :previous-state from
                                    :next-state to
                                    :payload (sar.i:dict
                                              "reason" (string-downcase
                                                        (symbol-name (or reason :normal))))))
      to)))

(defun ar-record-review (runtime candidate-id args)
  "Record one review result. Reviewers do not see each other's scores
before submitting; vetoes become explicit contrary evidence."
  (let ((candidate (or (gethash candidate-id (ar-candidates runtime))
                       (error 'research-core:validation-error
                              :message (format nil "unknown candidate: ~a" candidate-id)))))
    (push args (cdr (assoc :reviews candidate)))
    (when (gethash "veto" args)
      (push (format nil "reviewer veto: ~a" (gethash "veto" args))
            (cdr (assoc :evidence-against candidate))))))

;;; ------------------------------------------------------------- RLM wiring

(defvar *ar-stage-invoker* nil
  "Semantic stage invoker. NIL selects the default RLM harness
invocation; tests may bind it to a scripted function of (runtime run
request project stage) that performs the stage's semantic work through the
semantic tools and returns :succeeded or signals.")

(defun stage-name (stage)
  (string-downcase (symbol-name stage)))

(define-condition ar-stage-error (error)
  ((stage :initarg :stage :reader ar-stage-error-stage)
   (message :initarg :message :reader ar-stage-error-message))
  (:report (lambda (c s)
             (format s "ARADR stage ~a failed: ~a"
                     (ar-stage-error-stage c)
                     (ar-stage-error-message c)))))

(defun ar-run-candidates (runtime run-id)
  "All candidates belonging to RUN-ID, oldest first."
  (loop :for id :being :the :hash-keys :of (ar-candidates runtime)
          :using (hash-value candidate)
        :when (string= (cdr (assoc :run-id candidate)) run-id)
          :collect candidate))

(defun stage-budget-of (project)
  "Project budget -> one bounded RLM budget per stage invocation. The
pack defaults (4 model calls, 4 tool calls) are sized for smoke tests, so
real semantic stages get explicit headroom."
  (let ((budget (find-budget-profile (project-config-budget-name project))))
    (apply #'rlm:make-budget
           (append '(:max-model-calls 32
                      :max-tool-calls 32
                      :max-context-ops 12
                      :max-total-tokens 200000
                      :max-cost-usd 1.0d0)
                   (when budget
                     (list :time-limit
                           (min 600 (slot-value budget 'wall-clock-seconds))))))))

(defun ar-stage-requirement (stage)
  "The bounded stage requirement stated in the model context."
  (ecase stage
    (:request "deterministic artifact verification; no model calls")
    (:research "search the corpus and record findings with sources; then submit exactly one candidate with hypothesis, predicted result, falsifier, one primary evidence citation, one acceptance criterion, and one rejected alternative")
    (:adversarial-review "record exactly one independent adversarial review per candidate with numeric scores; veto only when the candidate must be rejected")
    (:analysis "record counterevidence and unresolved questions for every material contrary point raised by the reviews")
    (:design "derive the design strictly from the recorded run evidence in the context: every significant decision must cite concrete findings, sources, or candidate evidence from this run; mark anything not directly supported by evidence as an explicit inference; never state invariants, actors, or rejected alternatives that do not occur in the evidence")
    (:design-review "read the design note and record one design review with an explicit verdict")
    (:promotion-gate "deterministic gate; no model calls")))

(defun candidate-summary (candidate)
  (format nil "candidate ~a: hypothesis: ~a; falsifier: ~a"
          (cdr (assoc :id candidate))
          (cdr (assoc :hypothesis candidate))
          (cdr (assoc :falsifier candidate))))

(defun ar-stage-context-data (runtime run stage)
  "Stage-specific bounded data for the context: candidate summaries for
the review stages. DESIGN receives the bounded projection of the
canonical run evidence bundle instead (see make-ar-stage-context)."
  (when (member stage '(:adversarial-review :analysis :design-review))
    (let ((candidates (ar-run-candidates runtime (research-core:run-id run))))
      (when candidates
        (format nil "~{~a~^~%~}" (mapcar #'candidate-summary candidates))))))

(defun make-ar-stage-context (runtime run request project stage)
  "Bounded per-stage context builder: never the whole corpus, never an
ever-growing transcript. Project identity, question, stage requirements,
the project prompt profile, and stage data only. The DESIGN stage
receives the deterministic projection of the run evidence bundle so the
design is grounded in recorded evidence, not candidate prose alone."
  (rlm:make-context
   (format nil "project: ~a~%roam subtree: ~a~%question: ~a~%stage: ~a~%stage requirements: ~a~%~a~a"
           (project-config-id project)
           (project-config-roam-slug project)
           (research-core:request-question request)
           (stage-name stage)
           (ar-stage-requirement stage)
           (or (project-prompt-context runtime project stage) "")
           (or (if (eq stage :design)
                   (project-evidence-bundle
                    (run-evidence-bundle runtime run))
                   (ar-stage-context-data runtime run stage))
               ""))
   :max-bytes 8192))

(defun ar-context-directive ()
  "Runtime guidance for the opaque text context: exactly one alias
(input), search/slice only, it duplicates the task text, and the runtime's
effect-isolation policy is stated up front."
  (format nil "The opaque context alias is \"input\"; it contains only the stage metadata already stated in this task, so inspect it at most once with context_search. Never pass any other alias value. The runtime executes effectful (write) tool calls only when requested singly: request write tools one per turn, never combined with other tool calls in the same response."))

(defun ar-stage-query (runtime run request project stage)
  "The bounded task text for one stage. CL owns stage transitions: the
query never lets the model choose them."
  (declare (ignore runtime))
  (ecase stage
    (:request "REQUEST stage: deterministic artifact verification; no semantic work.")
    (:research
     (format nil "RESEARCH STAGE for project ~a, run ~a. Question: ~a. Search the corpus with search-research, record findings with record-finding, then call submit-candidate with run_id \"~a\", problem, hypothesis, predicted_result, falsifier, and the optional fields evidence, acceptance_criteria, rejected_alternatives. Do not finish with prose: your final action must be the submit-candidate tool call. ~a"
             (project-config-id project)
             (research-core:run-id run)
             (research-core:request-question request)
             (research-core:run-id run)
             (ar-context-directive)))
    (:adversarial-review
     (format nil "ADVERSARIAL REVIEW stage for project ~a, run ~a. For each candidate listed in the context call record-review exactly once with reviewer \"adversarial-reviewer\", numeric scores for evidence, architecture, novelty, feasibility, testability, leverage, and scope, and a veto reason only when the candidate must be rejected. CL owns candidate state; never modify it. ~a"
             (project-config-id project)
             (research-core:run-id run)
             (ar-context-directive)))
    (:analysis
     (format nil "ANALYSIS stage for project ~a, run ~a. For every material contrary point raised by the reviews, call record-counterevidence or record-unresolved-question on the research node. CL owns candidate state; never modify it. ~a"
             (project-config-id project)
             (research-core:run-id run)
             (ar-context-directive)))
    (:design
     (format nil "DESIGN stage for project ~a, run ~a. The context contains the run evidence projection (identity, candidates, findings, sources, counterevidence, research artifacts). Call record-design-note once with run_id ~a and a bounded design derived ONLY from that evidence: cite concrete findings, sources, or candidate evidence for every significant decision, and mark anything not directly supported as an explicit inference. Never state generic invariants, actors, or rejected alternatives that do not occur in the evidence. ~a"
             (project-config-id project)
             (research-core:run-id run)
             (research-core:run-id run)
             (ar-context-directive)))
    (:design-review
     (format nil "DESIGN REVIEW stage for run ~a. Read the design note, then call record-design-review once with run_id ~a, a bounded review, and an explicit verdict. ~a"
             (research-core:run-id run)
             (research-core:run-id run)
             (ar-context-directive)))
    (:promotion-gate "PROMOTION GATE stage: deterministic; no model calls.")))

(defun ar-default-stage-invoker (runtime run request project stage)
  "Default semantic stage invocation: one bounded direct-mode RLM run.
Transport/boot failures are retried once — they cost no tokens; semantic
failures are terminal."
  (let ((attempt 0))
    (loop
      (incf attempt)
      (handler-case
          (progn
            (ar-record-event runtime :rlm.started (research-core:run-id run)
                             :payload (sar.i:dict "stage" (stage-name stage)))
            (let ((rlm-result (rlm:run :query (ar-stage-query runtime run request project stage)
                                       :registry (ar-rlm-registry runtime)
                                       :context (make-ar-stage-context runtime run request project stage)
                                       :budget (stage-budget-of project)
                                       :provider (ar-provider runtime))))
              (cond
                ((eq (rlm:run-status rlm-result) :succeeded)
                 (ar-record-event runtime :rlm.completed (research-core:run-id run)
                                  :payload (sar.i:dict "stage" (stage-name stage)
                                                       "usage" (copy-usage (rlm:run-usage rlm-result))))
                 (return-from ar-default-stage-invoker :succeeded))
                (t
                 (ar-record-event runtime :rlm.failed (research-core:run-id run)
                                  :payload (sar.i:dict "stage" (stage-name stage)
                                                       "error" (format nil "~a" (rlm:run-error rlm-result))
                                                       "cause" (getf (rlm:run-error rlm-result) :cause)))
                 (error 'ar-stage-error :stage stage
                        :message (format nil "semantic backend failed: ~a"
                                         (rlm:run-error rlm-result)))))))
        (rlm:rlm-transport-error (condition)
          (when (>= attempt 2)
            (error 'ar-stage-error :stage stage
                   :message (format nil "semantic transport failed after retry: ~a" condition)))
          (ar-record-event runtime :rlm.started (research-core:run-id run)
                           :payload (sar.i:dict "stage" (stage-name stage)
                                                "retry" attempt))
          (sleep 1))))))

(defun ar-invoke-semantic-stage (runtime run request project stage)
  (unless (aradr:stage-requires-semantic-work-p stage)
    (error 'ar-stage-error :stage stage :message "stage does not require semantic work"))
  (funcall (or *ar-stage-invoker* #'ar-default-stage-invoker)
           runtime run request project stage))

;;; -------------------------------------------------------- ARADR scheduling

(defun ar-execute-stage (runtime run request project stage)
  "Execute one stage: semantic work where the stage requires it, and the
deterministic checks, completion requirements, and candidate lifecycle
transitions owned by CL. Ordinary checks never spend LLM calls. Semantic
tool activity is attributed to this run through *ar-current-run-id*."
  (let ((*ar-current-run-id* (research-core:run-id run)))
    (ecase stage
      (:request
       ;; deterministic: an issue-backed request must have its durable ARDR
       ;; transaction node before any semantic work starts
       (let ((ref (research-core:request-issue-ref request)))
         (when ref
           (unless (find-ardr-issue-transaction
                    (ar-store runtime)
                    (research-core:issue-ref-instance ref)
                    (research-core:issue-ref-repository ref)
                    (research-core:issue-ref-number ref))
             (error 'ar-stage-error :stage stage
                    :message "issue-backed request lacks its ARDR transaction node")))))
      (:research
       (ar-invoke-semantic-stage runtime run request project stage)
       (let ((candidates (ar-run-candidates runtime (research-core:run-id run))))
         (unless candidates
           (error 'ar-stage-error :stage stage
                  :message "research produced no candidate"))
         (dolist (candidate candidates)
           (when (eq (cdr (assoc :state candidate)) :queued)
             (ar-candidate-transition runtime (cdr (assoc :id candidate)) :exploring)))))
      (:adversarial-review
       (let ((candidates (ar-run-candidates runtime (research-core:run-id run)))
             (known-ids (loop :for id :being :the :hash-keys :of (ar-candidates runtime)
                              :collect id)))
         ;; deterministic checks first: no LLM calls spent on ordinary checks
         (dolist (candidate candidates)
           (aradr:check-required-fields
            candidate '(:problem :hypothesis :predicted-result :falsifier))
           (aradr:check-parent-ids candidate known-ids))
         ;; proposal complete -> validation passed -> entering virtual review
         (dolist (candidate candidates)
           (let ((id (cdr (assoc :id candidate))))
             (when (eq (cdr (assoc :state candidate)) :exploring)
               (ar-candidate-transition runtime id :proposing))
             (when (eq (cdr (assoc :state (gethash id (ar-candidates runtime))))
                       :proposing)
               (ar-candidate-transition runtime id :validating))))
         (ar-invoke-semantic-stage runtime run request project stage)
         (dolist (candidate (ar-run-candidates runtime (research-core:run-id run)))
           (unless (cdr (assoc :reviews candidate))
             (error 'ar-stage-error :stage stage
                    :message (format nil "candidate ~a has no adversarial review"
                                     (cdr (assoc :id candidate)))))
           (when (eq (cdr (assoc :state candidate)) :validating)
             (ar-candidate-transition runtime (cdr (assoc :id candidate))
                                      :virtual-review)))))
      (:analysis
       (ar-invoke-semantic-stage runtime run request project stage)
       (dolist (candidate (ar-run-candidates runtime (research-core:run-id run)))
         (when (eq (cdr (assoc :state candidate)) :virtual-review)
           (ar-candidate-transition runtime (cdr (assoc :id candidate)) :evaluated))))
      (:design
       ;; NO EVIDENCE -> NO DESIGN: deterministic gate evaluated on the
       ;; canonical run evidence bundle BEFORE any semantic work; a design
       ;; stage must refuse to advance without a research basis.
       (let ((problem (evidence-grounding-problem runtime run)))
         (when problem
           (error 'ar-stage-error :stage stage
                  :message (format nil "no-evidence-no-design: ~a" problem))))
       (ar-invoke-semantic-stage runtime run request project stage)
       ;; structural validation: a design does not count as valid merely
       ;; because its file exists
       (multiple-value-bind (valid reasons)
           (validate-design-node (ar-store runtime)
                                 (project-config-roam-slug project)
                                 (research-core:run-id run))
         (unless valid
           (error 'ar-stage-error :stage stage
                  :message (format nil "design validation failed: ~{~a~^; ~}" reasons)))))
      (:design-review
       (ar-invoke-semantic-stage runtime run request project stage)
       (unless (design-reviewed-p
                (design-node-path (ar-store runtime)
                                  (project-config-roam-slug project)
                                  (research-core:run-id run)))
         (error 'ar-stage-error :stage stage
                :message "design review not recorded"))))))

(defun ar-aggregate-review-score (reviews dimension)
  "Mean of one numeric review dimension across all recorded reviews; nil
when no review recorded a score for it."
  (let ((values (loop :for review :in reviews
                      :for value := (and (hash-table-p review)
                                         (gethash dimension review))
                      :when (numberp value) :collect value)))
    (when values
      (/ (reduce #'+ values) (length values)))))

(defparameter +promotion-thresholds+ '(:evidence-min 4 :architecture-min 4)
  "Deterministic promotion thresholds. A numeric score can never override
a veto.")

(defun ar-promotion-gate-stage (runtime run)
  "The deterministic promotion gate: run vetoes, design validity, and
the gate for every evaluated candidate using the aggregated review
scores. A candidate can never reach :promoted while its required design
stage is invalid or pending: the design is structurally validated first
and an invalid design denies every candidate of the run. Denial is a
recorded outcome, not a crash; the run completes without promoting when
the gate denies. Promotion never activates an implementation slot and
never implies implementation approval."
  (let* ((request (ar-get-request runtime (research-core:run-request-id run)))
         (project (and request
                       (find-project-config
                        (project-config-id
                         (research-core:request-project request)))))
         ;; project-specific policy: ARADR runs require a valid design;
         ;; methods without a design stage are exempt
         (design-required (and project (eq (project-config-method project) :aradr)))
         (design-reasons
           (when design-required
             (multiple-value-bind (valid reasons)
                 (validate-design-node (ar-store runtime)
                                       (project-config-roam-slug project)
                                       (research-core:run-id run))
               (unless valid reasons))))
         (promoted)
         (denied))
    (dolist (candidate (ar-run-candidates runtime (research-core:run-id run)))
      (let* ((id (cdr (assoc :id candidate)))
             (vetoes (aradr:run-vetoes candidate))
             (reviews (cdr (assoc :reviews candidate)))
             (evidence (ar-aggregate-review-score reviews "evidence"))
             (architecture (ar-aggregate-review-score reviews "architecture")))
        (cond
          (design-reasons
           (push (sar.i:dict "candidate" id
                             "reasons" (append (list "design validation failed")
                                               design-reasons))
                 denied))
          ((or (null evidence) (null architecture))
           (push (sar.i:dict "candidate" id
                             "reasons" (list "no recorded evidence/architecture review scores"))
                 denied))
          (t
           (handler-case
               (progn
                 (aradr:promotion-gate candidate vetoes
                                       (list (cons :evidence evidence)
                                             (cons :architecture architecture))
                                       +promotion-thresholds+)
                 (ar-candidate-transition runtime id :promoted)
                 (push id promoted))
             (aradr:promotion-denied (condition)
               (push (sar.i:dict "candidate" id
                                 "reasons" (aradr:promotion-denied-reason-list condition))
                     denied)))))))
    (when promoted
      (ar-record-event runtime :promotion.recorded (research-core:run-id run)
                       :payload (sar.i:dict "candidates" (nreverse promoted))))
    (when denied
      (ar-record-event runtime :promotion.denied (research-core:run-id run)
                       :payload (sar.i:dict "denials" (nreverse denied))))
    (values (nreverse promoted) (nreverse denied))))

(defun ar-run-stage-walk (runtime run request project)
  "Walk RUN through the canonical ARADR stages in order: REQUEST ->
RESEARCH -> ADVERSARIAL REVIEW -> ANALYSIS -> DESIGN -> DESIGN REVIEW ->
PROMOTION GATE. Stage transitions are owned by CL; the model performs
semantic work only when CL schedules a stage that requires it. Any stage
failure blocks the run fail-closed at :unresolved."
  (let ((stage :request)
        (result nil))
    (loop
      (ar-record-event runtime :stage.started (research-core:run-id run)
                       :payload (sar.i:dict "stage" (stage-name stage)))
      (handler-case
          (progn
            (if (eq stage :promotion-gate)
                (multiple-value-bind (promoted denied)
                    (ar-promotion-gate-stage runtime run)
                  (setf result (sar.i:dict "promoted" promoted "denied" denied)))
                (ar-execute-stage runtime run request project stage))
            (ar-record-event runtime :stage.completed (research-core:run-id run)
                             :payload (sar.i:dict "stage" (stage-name stage)))
            (when (eq stage :promotion-gate)
              (setf (research-core:run-status run) :completed)
              (ar-record-event runtime :run.completed (research-core:run-id run)
                               :next-state :completed
                               :payload (sar.i:dict "result" (or result "")))
              (return t))
            (setf stage (aradr:next-stage stage)))
        (error (condition)
          (ar-record-event runtime :stage.blocked (research-core:run-id run)
                           :payload (sar.i:dict "stage" (stage-name stage)
                                                "error" (princ-to-string condition)))
          (setf (research-core:run-status run) :unresolved)
          (ar-record-event runtime :run.completed (research-core:run-id run)
                           :next-state :unresolved)
          (return nil))))))

(defun ar-run-pending (runtime &key project-id)
  "Execute queued runs within budget by walking the canonical ARADR
stages with per-stage RLM invocations for semantic work. A run may
complete without promoting any candidate. Implementation is never
auto-started and approval remains PENDING."
  (dolist (run (ar-list-runs runtime :queued))
    (let* ((request (ar-get-request runtime
                                    (research-core:run-request-id run)))
           (project (and request
                         (find-project-config
                          (project-config-id
                           (research-core:request-project request))))))
      (when (and project (or (null project-id)
                             (string= project-id (project-config-id project))))
        (setf (research-core:run-status run) :running)
        (ar-record-event runtime :run.started (research-core:run-id run)
                         :previous-state :queued :next-state :running)
        (ar-run-stage-walk runtime run request project)))))

(defun copy-usage (usage)
  (when (listp usage)
    (let ((table (make-hash-table :test #'equal)))
      (loop :for (key value) :on usage :by #'cddr
            :when value
              :do (setf (gethash (string-downcase (symbol-name key)) table) value))
      table)))

;;; ------------------------------------------------------------ promotion

(defun promote-candidate (runtime candidate-id scores thresholds)
  "Run ARADR promotion guards for CANDIDATE and, on success, record the
promotion transition to :promoted. Denials raise aradr:promotion-denied
with all reasons and leave the candidate untouched. A candidate whose
run requires a design stage (project method :aradr) cannot be promoted
while its design node is structurally invalid. Promotion never activates
an implementation slot and never implies implementation approval."
  (let* ((candidate (or (gethash candidate-id (ar-candidates runtime))
                        (error 'research-core:validation-error
                               :message (format nil "unknown candidate: ~a" candidate-id))))
         (vetoes (aradr:run-vetoes candidate))
         (run (and (cdr (assoc :run-id candidate))
                   (ar-get-run runtime (cdr (assoc :run-id candidate)))))
         (request (and run (ar-get-request runtime
                                           (research-core:run-request-id run))))
         (project (and request
                       (find-project-config
                        (project-config-id
                         (research-core:request-project request)))))
         (design-reasons
           (when (and project (eq (project-config-method project) :aradr) run)
             (multiple-value-bind (valid reasons)
                 (validate-design-node (ar-store runtime)
                                       (project-config-roam-slug project)
                                       (research-core:run-id run))
               (unless valid
                 (append (list "design validation failed") reasons))))))
    (when design-reasons
      (error 'aradr:promotion-denied :reasons design-reasons))
    (aradr:promotion-gate candidate vetoes scores thresholds)
    (ar-candidate-transition runtime candidate-id :promoted)))

;;; -------------------------------------------------------------- approval

(defun record-research-approval (runtime path state actor evidence)
  "Persist an explicit approval decision onto a research node. Lifecycle
and approval remain independent dimensions: this never changes #+status:.
Partial canonical metadata is rejected by the validators."
  (let* ((full (ensure-path-in-store%/ (ar-store runtime) path))
         (text (uiop:read-file-string full)))
    (unless (member state '("APPROVED" "REJECTED" "PENDING") :test #'string=)
      (error 'research-core:validation-error
             :message (format nil "invalid approval state: ~a" state)))
    (unless (and evidence (plusp (length (string-trim " " evidence))))
      (error 'research-core:validation-error :message "approval requires evidence"))
    (let* ((base-commit "uncommitted")
           (base-blob (ironclad:byte-array-to-hex-string
                       (ironclad:digest-sequence
                        :sha256
                        (babel:string-to-octets text :encoding :utf-8))))
           (updated text))
      (dolist (field '("approval_schema" "approval_state" "approval_actor"
                       "approval_evidence" "approval_base_commit"
                       "approval_base_blob" "approval_decided_at"))
        (setf updated
              (upsert-org-keyword
               updated field
               (cdr (assoc field (approval-alist state actor evidence
                                                 base-commit base-blob))))))
      (with-open-file (out full :direction :output :if-exists :supersede
                                :external-format :utf-8)
        (write-string updated out))
      (ar-record-event runtime :approval.recorded
                       (enough-namestring full (ar-root runtime))
                       :next-state (intern (string-downcase state) :keyword)
                       :evidence (list evidence))
      full)))

(defun approval-alist (state actor evidence base-commit base-blob)
  (list (cons "approval_schema" +approval-schema+)
        (cons "approval_state" state)
        (cons "approval_actor" actor)
        (cons "approval_evidence" evidence)
        (cons "approval_base_commit" base-commit)
        (cons "approval_base_blob" base-blob)
        (cons "approval_decided_at"
              (if (string= state "PENDING") "NONE" (sar.i:now-iso8601)))))

(defmethod research-core:runtime-state ((runtime ar-runtime))
  "Read-model projection for dashboards: configured projects plus live
request/run state."
  (let ((projects)
        (runs)
        (requests))
    (loop :for id :being :the :hash-keys :of (project-configs)
            :using (hash-value config)
          :do (push (sar.i:dict "id" id
                                "roam_subtree" (project-config-roam-slug config)
                                "rlm_mode" (string-downcase
                                            (symbol-name (project-config-rlm-mode config))))
                    projects))
    (loop :for run :being :the :hash-values :of (ar-runs-slot runtime)
          :do (push (sar.i:dict "id" (research-core:run-id run)
                                "status" (string-downcase
                                          (symbol-name (research-core:run-status run))))
                    runs))
    (loop :for request :being :the :hash-values :of (ar-requests-slot runtime)
          :do (push (sar.i:dict "id" (research-core:request-id request)
                                "question" (research-core:request-question request)
                                "status" (string-downcase
                                          (symbol-name (research-core:request-status request))))
                    requests))
    (sar.i:dict "projects" (nreverse projects)
                "runs" (nreverse runs)
                "requests" (nreverse requests))))
