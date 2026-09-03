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
                    :reader ar-revision-counts)))

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
                   :event-store (make-jsonl-event-store
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

(defun ar-submit-research (runtime project question &key (source :api) issue-ref)
  "API-origin request: canonical request -> issue in the configured
research repo -> ARDR issue transaction -> run. Returns
(values request run issue-number existing-issue-p)."
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
                                            "source" (string-downcase (symbol-name source))))
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
                         :payload (sar.i:dict "created" (not created-p))))
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
            (progn
              (ar-submit-research runtime config
                                  (or (extract-question body)
                                      (and (stringp title) title)
                                      "No question recorded.")
                                  :source :forgejo
                                  :issue-ref (research-core:make-issue-ref
                                              :forgejo instance repository number))
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
                            falsifier &optional parent-ids)
  "Create one bounded candidate per the approved contract and record the
candidate.created event at :queued."
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
                (cons :evidence-for nil)
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

(defun ar-rlm-run (runtime run request)
  "Invoke Prolog-RLM :direct for RUN through the generic harness. The
semantic tool registry and the bounded context belong to CL; the model
receives only the bounded task text, the opaque context, and the semantic
tool set."
  (let* ((project (find-project-config
                   (research-core:project-id (research-core:request-project request))))
         (context (make-ar-context runtime request project)))
    (rlm:run :query (format nil
                            "RESEARCH STAGE for project ~a. Question: ~a. Use the semantic tools to search the corpus and record findings with sources; then submit-candidate with hypothesis, predicted result, and falsifier."
                            (project-config-id project)
                            (research-core:request-question request))
             :registry (ar-rlm-registry runtime)
             :context context
             :provider (ar-provider runtime))))

(defun make-ar-context (runtime request project)
  "Bounded context builder: never the whole corpus, never an ever-growing
transcript. Question, project identity, and stage requirements only."
  (declare (ignore runtime))
  (rlm:make-context
   (format nil "project: ~a~%roam subtree: ~a~%stage requirements: submit one candidate with hypothesis, predicted result, falsifier, and evidence~%question: ~a~%"
           (project-config-id project)
           (project-config-roam-slug project)
           (research-core:request-question request))
   :max-bytes 8192))

(defun budget-spec-of (project)
  "Project budget -> RLM budget knobs."
  (let ((budget (find-budget-profile (project-config-budget-name project))))
    (when budget
      (list :max-total-tokens 200000
            :wall-clock (min 600 (slot-value budget 'wall-clock-seconds))))))

;;; -------------------------------------------------------- ARADR scheduling

(defun ar-run-pending (runtime &key project-id)
  "Execute queued runs within budget: advance ARADR stages, schedule
semantic work through the RLM harness, record stage and run events. A run
may complete without promoting any candidate. Implementation is never
auto-started and approval remains PENDING."
  (dolist (run (ar-list-runs runtime :queued))
    (let* ((request (ar-get-request runtime
                                    (research-core:run-request-id run)))
           (project (find-project-config
                     (research-core:project-id (research-core:request-project request)))))
      (when (and project (or (null project-id)
                             (string= project-id (project-config-id project))))
        (setf (research-core:run-status run) :running)
        (ar-record-event runtime :run.started (research-core:run-id run)
                         :previous-state :queued :next-state :running)
        (ar-record-event runtime :stage.started (research-core:run-id run)
                         :payload (sar.i:dict "stage" "research"))
        (let ((rlm-result (ar-rlm-run runtime run request)))
          (cond
            ((eq (rlm:run-status rlm-result) :succeeded)
             (ar-record-event runtime :rlm.started (research-core:run-id run))
             (ar-record-event runtime :rlm.completed (research-core:run-id run)
                              :payload (sar.i:dict
                                        "usage" (rc.j:stringify-value
                                                 (copy-usage (rlm:run-usage rlm-result)))))
             (ar-record-event runtime :stage.completed (research-core:run-id run)
                              :payload (sar.i:dict "stage" "research"))
             (setf (research-core:run-status run) :completed)
             (ar-record-event runtime :run.completed (research-core:run-id run)
                              :next-state :completed
                              :payload (sar.i:dict "result" (or (rlm:run-result rlm-result) ""))))
            (t
             ;; semantic backend failed structurally: the run records the
             ;; failure and finishes without promotion (fail closed)
             (ar-record-event runtime :rlm.failed (research-core:run-id run)
                              :payload (sar.i:dict "error" (format nil "~a" (rlm:run-error rlm-result))))
             (ar-record-event runtime :stage.blocked (research-core:run-id run)
                              :payload (sar.i:dict "stage" "research"))
             (setf (research-core:run-status run) :unresolved)
             (ar-record-event runtime :run.completed (research-core:run-id run)
                              :next-state :unresolved))))))))

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
promotion. Denials raise aradr:promotion-denied with all reasons. Promotion
never activates an implementation slot and never implies implementation
approval."
  (let* ((candidate (or (gethash candidate-id (ar-candidates runtime))
                        (error 'research-core:validation-error
                               :message (format nil "unknown candidate: ~a" candidate-id))))
         (vetoes (aradr:run-vetoes candidate)))
    (aradr:promotion-gate candidate vetoes scores thresholds)))

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
