;;;; protocols.lisp --- core generic operations and the in-memory runtime.
;;;;
;;;; CLI, HTTP, cron, and agents call these operations; none of them may
;;;; duplicate workflow logic. The runtime wires projects, stores, and the
;;;; RLM registry together and owns request/run bookkeeping and event
;;;; recording. ARADR semantics live in starintel-aradr; this layer emits
;;;; the events those semantics produce.

(in-package #:research-core)

(defgeneric find-project (runtime project-id)
  (:documentation "Resolve a trusted-init project by ID."))

(defgeneric submit-research (runtime principal project question &key source issue-ref)
  (:documentation "Create a research request and run, emit events, return
(values request run). Callers must hold :|research:submit|."))

(defgeneric get-request (runtime request-id))

(defgeneric get-run (runtime run-id))

(defgeneric list-runs (runtime &key project-id status))

(defgeneric cancel-run (runtime principal run-id)
  (:documentation "Cancel a run; caller must hold :|research:cancel|."))

(defgeneric record-approval (runtime principal artifact-id state evidence)
  (:documentation "Record an explicit approval decision. State is one of
:pending/:approved/:rejected; never derived from lifecycle status."))

(defgeneric promote (runtime principal candidate-id)
  (:documentation "Run ARADR promotion guards; cannot force state."))

(defgeneric process-forgejo-event (runtime event)
  (:documentation "Normalize a verified Forgejo event into a request,
creating it only if the qualified issue identity is new (idempotent)."))

(defgeneric run-pending (runtime &key project-id)
  (:documentation "Execute pending runs within budget."))

(defgeneric record-event (runtime type entity-id &key previous-state next-state
                                    actor payload evidence)
  (:documentation "Record a domain event on the runtime's event store."))

(defgeneric runtime-state (runtime)
  (:documentation "Read-model projection for dashboards."))

;;; -------------------------------------------------------------- artifacts

(defclass artifact-store ()
  ()
  (:documentation "Protocol for canonical artifact storage (e.g. the
existing Org-roam corpus). Replaceable by StarIntel-native stores."))

(defgeneric write-artifact (store path content &key)
  (:documentation "Store CONTENT under PATH; storage adapters enforce
root confinement and rejection of ../ escapes."))

(defgeneric read-artifact (store path &key)
  (:documentation "Read the artifact at PATH."))

(defgeneric search-artifacts (store query &key project limit)
  (:documentation "Lexical artifact search preserving current semantics."))

;;; ---------------------------------------------------------------- runtime

(defclass runtime ()
  ((projects :initarg :projects :initform (make-hash-table :test #'equal)
             :reader runtime-projects)
   (store :initarg :store :accessor runtime-store)
   (registry :initarg :registry :initform nil :accessor runtime-registry)
   (requests :initform (make-hash-table :test #'equal) :accessor runtime-requests)
   (runs :initform (make-hash-table :test #'equal) :accessor runtime-runs)
   (lock :initform (bt:make-lock) :reader runtime-lock)))

(defun make-runtime (&key projects store registry)
  "RUNTIME requires an injected event STORE. PROJECTS is a list of
project instances from the trusted init DSL."
  (unless store
    (error 'validation-error :message "runtime requires an event store"))
  (let ((runtime (make-instance 'runtime :store store :registry registry)))
    (dolist (project projects)
      (when (gethash (project-id project) (runtime-projects runtime))
        (error 'validation-error
               :message (format nil "duplicate project id: ~a" (project-id project))))
      (setf (gethash (project-id project) (runtime-projects runtime)) project))
    runtime))

(defmethod find-project ((runtime runtime) project-id)
  (gethash project-id (runtime-projects runtime)))

(defmethod record-event ((runtime runtime) type entity-id &rest keys
                         &key previous-state next-state actor payload evidence)
  (declare (ignore previous-state next-state actor payload evidence))
  (append-event (runtime-store runtime)
                (apply #'make-event type entity-id keys)))

(defmethod get-request ((runtime runtime) request-id)
  (gethash request-id (runtime-requests runtime)))

(defmethod get-run ((runtime runtime) run-id)
  (gethash run-id (runtime-runs runtime)))

(defmethod list-runs ((runtime runtime) &key project-id status)
  (let ((runs))
    (loop :for run :being :the :hash-values :of (runtime-runs runtime)
          :do (when (and (or (null project-id)
                             (string= (project-id (run-project run)) project-id))
                         (or (null status) (eq (run-status run) status)))
                (push run runs)))
    (sort runs #'string< :key #'run-id)))

(defmethod submit-research ((runtime runtime) principal project question
                            &key (source :api) issue-ref)
  (check-capability principal :|research:submit|)
  (unless (and question (plusp (length (string-trim " " question))))
    (error 'validation-error :message "research question must be non-empty"))
  (bt:with-lock-held ((runtime-lock runtime))
    (let* ((request-id (rc.i:short-id "req"))
           (run-id (rc.i:short-id "run"))
           (request (make-instance 'research-request
                                   :id request-id
                                   :project project
                                   :question question
                                   :source source
                                   :issue-ref issue-ref
                                   :status :received
                                   :run-ids (list run-id)
                                   :created-at (rc.i:now-iso8601)))
           (run (make-instance 'research-run
                               :id run-id
                               :request-id request-id
                               :project project
                               :status :queued
                               :budget (project-budget project)
                               :created-at (rc.i:now-iso8601))))
      (append-event (runtime-store runtime)
                    (make-event :request.received request-id
                                :next-state :received
                                :actor (principal-id principal)
                                :payload (rc.i:dict "question" question
                                               "source" (string-downcase (symbol-name source)))))
      (setf (gethash request-id (runtime-requests runtime)) request)
      (setf (gethash run-id (runtime-runs runtime)) run)
      (append-event (runtime-store runtime)
                    (make-event :run.created run-id
                                :next-state :queued
                                :actor (principal-id principal)
                                :payload (rc.i:dict "request_id" request-id)))
      (values request run))))

(defmethod cancel-run ((runtime runtime) principal run-id)
  (check-capability principal :|research:cancel|)
  (let ((run (gethash run-id (runtime-runs runtime))))
    (unless run
      (error 'validation-error :message (format nil "unknown run: ~a" run-id)))
    (when (member (run-status run) '(:queued :running))
      (setf (run-status run) :cancelled)
      (append-event (runtime-store runtime)
                    (make-event :run.cancelled run-id
                                :previous-state (run-status run)
                                :next-state :cancelled
                                :actor (principal-id principal))))
    run))

(defmethod record-approval ((runtime runtime) principal artifact-id state evidence)
  ;; Approval and lifecycle are different facts; this records an explicit
  ;; human decision only. The default capability is research approval;
  ;; design approvals use :design:approve.
  (check-capability principal :|research:approve|)
  (unless (member state '(:pending :approved :rejected))
    (error 'validation-error
           :message (format nil "invalid approval state: ~a" state)))
  (unless (and evidence (plusp (length (string-trim " " evidence))))
    (error 'validation-error :message "approval requires evidence"))
  (append-event (runtime-store runtime)
                (make-event :approval.recorded artifact-id
                            :next-state state
                            :actor (principal-id principal)
                            :evidence (list evidence)
                            :payload (rc.i:dict "state" (string-downcase (symbol-name state))))))

(defmethod runtime-state ((runtime runtime))
  (let ((projects)
        (runs)
        (requests))
    (loop :for project :being :the :hash-values :of (runtime-projects runtime)
          :do (push (rc.i:dict "id" (project-id project)
                          "display_name" (project-display-name project))
                    projects))
    (loop :for run :being :the :hash-values :of (runtime-runs runtime)
          :do (push (rc.i:dict "id" (run-id run)
                          "status" (string-downcase (symbol-name (run-status run))))
                    runs))
    (loop :for request :being :the :hash-values :of (runtime-requests runtime)
          :do (push (rc.i:dict "id" (request-id request)
                          "status" (string-downcase (symbol-name (request-status request))))
                    requests))
    (rc.i:dict "projects" (nreverse projects)
          "runs" (nreverse runs)
          "requests" (nreverse requests))))
