;;;; api.lisp --- /api/v1 routes over the application API.
;;;;
;;;; Thin projection: authenticate, authorize, call the runtime, project
;;;; the result as JSON. SSE streams domain events, never raw model stdout.

(in-package #:starintel-auto-research-web)

(defparameter *runtime* nil
  "The ar-runtime this API projects. Set by trusted init before start.")

(defparameter *acceptor* nil)

(defun json-ok (object &optional (status 200))
  (setf (hunchentoot:content-type*) "application/json")
  (setf (hunchentoot:return-code*) status)
  (jz:stringify object))

(defun json-error (status message &key (code nil code-p))
  (setf (hunchentoot:content-type*) "application/json")
  (setf (hunchentoot:return-code*) status)
  (jz:stringify (alexandria:plist-hash-table
                 (append (list "error" message)
                         (when code-p (list "code" code)))
                 :test #'equal)))

(defun route-params (pattern path)
  "Match PATH against PATTERN; returns the list of captured group values
or nil. Used by handle-api clauses."
  (multiple-value-bind (match groups)
      (ppcre:scan-to-strings pattern path)
    (when match (coerce groups 'list))))

(defmacro with-auth ((capability) &body body)
  `(handler-case
       (progn (require-capability ,capability) ,@body)
     (rc:authentication-error (condition)
       (json-error 401 (format nil "~a" condition)))
     (rc:authorization-error (condition)
       (json-error 403 (format nil "~a" condition)))))

(defun dispatch ()
  "Top-level /api/v1 dispatcher."
  (handler-case
      (let ((path (hunchentoot:script-name*))
            (method (hunchentoot:request-method*)))
        (cond
          ;; runtime -----------------------------------------------------
          ((and (string= path "/api/v1/health") (eq method :get))
           (json-ok (alexandria:plist-hash-table
                     (list "status" "ok" "time" (rci:now-iso8601))
                     :test #'equal)))
          ((and (string= path "/api/v1/ready") (eq method :get))
           (json-ok (alexandria:plist-hash-table
                     (list "ready" (and *runtime* t)) :test #'equal)))
          ((and (string= path "/api/v1/runtime") (eq method :get))
           (with-auth (:|runtime:inspect|)
             (json-ok (rc:runtime-state *runtime*))))
          ;; hooks (webhook auth is separate) ----------------------------
          ((and (string= path "/api/v1/hooks/forgejo") (eq method :post))
           (handle-forgejo-hook))
          ;; everything else requires authentication ---------------------
          ((ppcre:scan "^/api/v1/" path)
           (handle-api path method))
          (t (json-error 404 "not found"))))
    (rc:authentication-error (condition)
      (json-error 401 (format nil "~a" condition)))
    (rc:authorization-error (condition)
      (json-error 403 (format nil "~a" condition)))
    (rc:validation-error (condition)
      (json-error 400 (format nil "~a" condition)))
    (rc:core-error (condition)
      (json-error 500 (format nil "~a" condition)))
    (error (condition)
      (json-error 500 (format nil "~a" condition)))))

(defun read-json-body ()
  (let ((body (hunchentoot:raw-post-data :force-text t)))
    (when (and body (plusp (length body)))
      (let ((parsed (jz:parse body)))
        (typecase parsed
          (hash-table parsed)
          (t (alexandria:plist-hash-table (list "question" (princ-to-string parsed))
                                          :test #'equal)))))))

(defun project-config-or-404 (project-id)
  (or (ar:find-project-config project-id)
      (error 'rc:validation-error
             :message (format nil "unknown project: ~a" project-id))))

(defun handle-run-pending ()
  "Trigger ar-run-pending for queued runs. The stage walk executes in a
worker thread (a full walk may take minutes); the response is 202 and
progress is observable through run events / SSE. An optional body
{\"project_id\": \"...\"} scopes the walk to one project."
  (with-auth (:|runtime:inspect|)
    (unless (eq (hunchentoot:request-method*) :post)
      (return-from handle-run-pending (json-error 405 "method not allowed")))
    (let* ((body (ignore-errors (read-json-body)))
           (project-id (and body (gethash "project_id" body))))
      (bt:make-thread
       (lambda ()
         (handler-case
             (if project-id
                 (ar:ar-run-pending *runtime* :project-id project-id)
                 (ar:ar-run-pending *runtime*))
           (error (condition)
             (format *error-output* "~&run-pending worker failed: ~a~%" condition))))
       :name "ar-run-pending-worker")
      (setf (hunchentoot:return-code*) 202)
      (json-ok (alexandria:plist-hash-table
                (list "started" t
                      "project_id" (or project-id "all"))
                :test #'equal)
               202))))

(defun project-resource (project-id resource method)
  (with-auth (:|research:read|)
    (project-config-or-404 project-id)
    (cond
      ((string= resource "research")
       (if (eq method :post)
           (handle-submit-research project-id)
           (json-ok (alexandria:plist-hash-table
                     (list "research" (ar:org-lexical-search
                                       (ar:ar-store *runtime*)
                                       (or (hunchentoot:get-parameter "q") project-id)
                                       :project project-id))
                     :test #'equal))))
      ((string= resource "runs")
       (json-ok (runs-view)))
      ((string= resource "designs")
       (json-ok (alexandria:plist-hash-table (list "designs" nil) :test #'equal)))
      ((string= resource "issues")
       (json-ok (alexandria:plist-hash-table (list "issues" nil) :test #'equal)))
      ((string= resource "artifacts")
       (json-ok (alexandria:plist-hash-table (list "artifacts" nil) :test #'equal)))
      ((string= resource "dashboard")
       (json-ok (rc:runtime-state *runtime*))))))

(defun handle-api (path method)
  (cond
    ;; projects ---------------------------------------------------------
    ((string= path "/api/v1/projects")
     (with-auth (:|research:read|)
       (unless (eq method :get) (return-from handle-api (json-error 405 "method not allowed")))
       (json-ok (projects-view))))
    ((route-params "^/api/v1/projects/([^/]+)$" path)
     (destructuring-bind (project-id) (route-params "^/api/v1/projects/([^/]+)$" path)
       (with-auth (:|research:read|)
         (unless (eq method :get) (return-from handle-api (json-error 405 "method not allowed")))
         (project-config-or-404 project-id)
         (json-ok (project-view project-id)))))
    ((route-params "^/api/v1/projects/([^/]+)/(research|designs|runs|issues|artifacts|dashboard)$" path)
     (destructuring-bind (project-id resource)
         (route-params "^/api/v1/projects/([^/]+)/(research|designs|runs|issues|artifacts|dashboard)$" path)
       (project-resource project-id resource method)))
    ((string= path "/api/v1/run-pending")
     (handle-run-pending))
    ;; requests and runs --------------------------------------------------
    ((and (string= path "/api/v1/requests") (eq method :get))
     (with-auth (:|research:read|)
       (json-ok (requests-view))))
    ((route-params "^/api/v1/requests/([^/]+)$" path)
     (destructuring-bind (request-id) (route-params "^/api/v1/requests/([^/]+)$" path)
       (with-auth (:|research:read|)
         (unless (eq method :get) (return-from handle-api (json-error 405 "method not allowed")))
         (let ((request (ar:ar-get-request *runtime* request-id)))
           (unless request (return-from handle-api (json-error 404 "unknown request")))
           (json-ok (request-view request))))))
    ((and (string= path "/api/v1/runs") (eq method :get))
     (with-auth (:|research:read|)
       (json-ok (runs-view))))
    ((route-params "^/api/v1/runs/([^/]+)(?:/(cancel|events|events/stream|candidates|artifacts|reviews|executions))?$" path)
     (destructuring-bind (run-id &optional action)
         (route-params "^/api/v1/runs/([^/]+)(?:/(cancel|events|events/stream|candidates|artifacts|reviews|executions))?$" path)
       (handle-run-resource run-id action method)))
    ;; candidates ---------------------------------------------------------
    ((route-params "^/api/v1/candidates/([^/]+)(/reviews)?$" path)
     (destructuring-bind (candidate-id &optional sub)
         (route-params "^/api/v1/candidates/([^/]+)(/reviews)?$" path)
       (with-auth (:|research:read|)
         (json-ok (candidate-view candidate-id (not (null sub)))))))
    ;; artifacts and search ----------------------------------------------
    ((and (route-params "^/api/v1/artifacts/(.+?)(/source)?$" path)
          (eq method :get))
     (destructuring-bind (artifact-id &optional source-p)
         (route-params "^/api/v1/artifacts/(.+?)(/source)?$" path)
       (with-auth (:|research:read|)
         (handle-artifact artifact-id (not (null source-p))))))
    ((and (string= path "/api/v1/search") (eq method :get))
     (with-auth (:|research:read|)
       (let ((q (hunchentoot:get-parameter "q"))
             (project (hunchentoot:get-parameter "project")))
         (unless q (return-from handle-api (json-error 400 "missing q")))
         (json-ok (search-view q project)))))
    ;; approval and promotion --------------------------------------------
    ((and (route-params "^/api/v1/artifacts/(.+?)/(approval|promote)$" path)
          (eq method :post))
     (destructuring-bind (artifact-id action)
         (route-params "^/api/v1/artifacts/(.+?)/(approval|promote)$" path)
       (handle-artifact-action artifact-id action)))
    ;; dashboard ---------------------------------------------------------
    ((string= path "/api/v1/dashboard")
     (with-auth (:|research:read|)
       (json-ok (rc:runtime-state *runtime*))))
    (t (json-error 404 "not found"))))

(defun handle-run-resource (run-id action method)
  (with-auth (:|research:read|)
    (let ((run (ar:ar-get-run *runtime* run-id)))
      (unless run (return-from handle-run-resource (json-error 404 "unknown run")))
      (cond
        ((null action)
         (unless (eq method :get) (return-from handle-run-resource (json-error 405 "method not allowed")))
         (json-ok (run-view run)))
        ((string= action "cancel")
         (unless (eq method :post) (return-from handle-run-resource (json-error 405 "method not allowed")))
         (require-capability :|research:cancel|)
         (if (member (rc:run-status run) '(:queued :running))
             (progn
               (setf (rc:run-status run) :cancelled)
               (json-ok (alexandria:plist-hash-table
                         (list "id" run-id "status" "cancelled") :test #'equal)))
             (json-error 409 "run is not cancellable")))
        ((string= action "events")
         (unless (eq method :get) (return-from handle-run-resource (json-error 405 "method not allowed")))
         (json-ok (events-view run-id)))
        ((string= action "events/stream")
         (unless (eq method :get) (return-from handle-run-resource (json-error 405 "method not allowed")))
         (sse-events run-id))
        ((member action '("candidates" "artifacts" "reviews" "executions") :test #'string=)
         (unless (eq method :get) (return-from handle-run-resource (json-error 405 "method not allowed")))
         (json-ok (run-sub-view run-id action)))))))

(defun handle-submit-research (project-id)
  (with-auth (:|research:submit|)
    (project-config-or-404 project-id)
    (let* ((body (read-json-body))
           (question (and body (gethash "question" body))))
      (unless (and question (stringp question))
        (return-from handle-submit-research (json-error 400 "body must contain a bounded question")))
      (multiple-value-bind (request run issue-number existing-p)
          (ar:ar-submit-research *runtime*
                                 (ar:find-project-config project-id)
                                 question
                                 :source :api)
        (declare (ignore request))
        (json-ok (alexandria:plist-hash-table
                  (list "status" "accepted"
                        "run_id" (rc:run-id run)
                        "request_status" (string-downcase (symbol-name (rc:run-status run)))
                        "forgejo_issue" (and issue-number (format nil "~a" issue-number))
                        "issue_existed" existing-p)
                  :test #'equal)
                 202)))))

(defun handle-artifact (artifact-id source-p)
  (let* ((store (ar:ar-store *runtime*))
         (path (merge-pathnames artifact-id (ar:org-roam-root store))))
    (unless (and (uiop:subpathp path (pathname (ar:org-roam-root store)))
                 (probe-file path))
      (return-from handle-artifact (json-error 404 "unknown artifact")))
    (if source-p
        (progn
          (setf (hunchentoot:content-type*) "text/org; charset=utf-8")
          (uiop:read-file-string path))
        (multiple-value-bind (text keywords approval lifecycle)
            (ar:read-research-node path)
          (declare (ignore text))
          (json-ok (alexandria:plist-hash-table
                    (list "id" artifact-id
                          "title" (gethash "title" keywords "")
                          "description" (gethash "description" keywords "")
                          "status" (gethash "status" keywords "")
                          "approval_state" (string-downcase (symbol-name approval))
                          "lifecycle" (string-downcase (symbol-name lifecycle)))
                    :test #'equal))))))

(defun handle-artifact-action (artifact-id action)
  (if (string= action "approval")
      (progn
        (require-capability :|research:approve|)
        (let ((body (read-json-body)))
          (ar:record-research-approval
           *runtime* artifact-id
           (or (and body (gethash "state" body)) "PENDING")
           (rc:principal-id (authenticate))
           (or (and body (gethash "evidence" body))
               (error 'rc:validation-error :message "approval requires evidence")))
          (json-ok (alexandria:plist-hash-table
                    (list "recorded" artifact-id "approval_state"
                          (string-downcase (or (and body (gethash "state" body)) "PENDING")))
                    :test #'equal))))
      ;; promote: runs ARADR guards, cannot force state
      (progn
        (require-capability :|research:approve|)
        (let* ((body (read-json-body))
               (candidate-id (and body (gethash "candidate_id" body))))
          (unless candidate-id
            (return-from handle-artifact-action (json-error 400 "body must contain candidate_id")))
          (handler-case
              (progn
                (ar:promote-candidate *runtime* candidate-id nil
                                      (list :evidence-min 4 :architecture-min 4))
                (json-ok (alexandria:plist-hash-table
                          (list "promoted" candidate-id) :test #'equal)))
            (aradr:promotion-denied (condition)
              (json-error 409 (format nil "~a" condition))))))))

(defun handle-forgejo-hook ()
  "Normalize a verified webhook event into the application layer. Webhook
verification is separate from normal API authentication: the payload must
carry the configured secret."
  (let* ((body (hunchentoot:raw-post-data :force-text t))
         (payload (and body (jz:parse body))))
    (unless payload
      (return-from handle-forgejo-hook (json-error 400 "missing payload")))
    (let ((table (if (hash-table-p payload) payload (make-hash-table :test #'equal)))
          (configured (uiop:getenv "FORGEJO_WEBHOOK_SECRET")))
      ;; verification fails closed: an unconfigured secret rejects every
      ;; webhook, and the presented secret must equal the configured one
      (unless (and configured
                   (plusp (length configured))
                   (string= (gethash "secret" table "") configured))
        (return-from handle-forgejo-hook (json-error 401 "webhook verification failed")))
      (handler-case
          (multiple-value-bind (result created-p)
              (rc:process-forgejo-event *runtime* table)
            (json-ok (alexandria:plist-hash-table
                      (list "result" (if created-p "created" "existing")
                            "detail" (princ-to-string (if (listp result)
                                                          (second result)
                                                          result)))
                      :test #'equal)
                     (if created-p 202 200)))
        (rc:validation-error (condition)
          (json-error 400 (format nil "~a" condition)))))))

;;; ------------------------------------------------------------- views

(defun project-config-view (id config)
  (alexandria:plist-hash-table
   (list "id" id
         "roam_subtree" (ar:project-config-roam-slug config)
         "method" (string-downcase (symbol-name (ar:project-config-method config)))
         "rlm_mode" (string-downcase (symbol-name (ar:project-config-rlm-mode config))))
   :test #'equal))

(defun projects-view ()
  (let ((projects))
    (loop :for id :being :the :hash-keys :of (ar:project-configs)
            :using (hash-value config)
          :do (push (project-config-view id config) projects))
    (alexandria:plist-hash-table (list "projects" (nreverse projects)) :test #'equal)))

(defun project-view (id)
  (project-config-view id (ar:find-project-config id)))

(defun requests-view ()
  (let ((requests))
    (loop :for request :being :the :hash-values :of (ar:ar-requests-slot *runtime*)
          :do (push (request-view request) requests))
    (alexandria:plist-hash-table (list "requests" (nreverse requests)) :test #'equal)))

(defun request-view (request)
  (alexandria:plist-hash-table
   (list "id" (rc:request-id request)
         "project" (rc:project-id (rc:request-project request))
         "question" (rc:request-question request)
         "source" (string-downcase (symbol-name (rc:request-source request)))
         "status" (string-downcase (symbol-name (rc:request-status request)))
         "created_at" (rc:request-created-at request))
   :test #'equal))

(defun runs-view ()
  (let ((runs))
    (loop :for run :being :the :hash-values :of (ar:ar-runs-slot *runtime*)
          :do (push (run-view run) runs))
    (alexandria:plist-hash-table (list "runs" (nreverse runs)) :test #'equal)))

(defun run-view (run)
  (alexandria:plist-hash-table
   (list "id" (rc:run-id run)
         "request_id" (rc:run-request-id run)
         "status" (string-downcase (symbol-name (rc:run-status run)))
         "created_at" (rc:run-created-at run))
   :test #'equal))

(defun events-view (run-id)
  (let ((events (rc:replay-events (ar:ar-event-store *runtime*)
                                  :entity-id run-id)))
    (alexandria:plist-hash-table
     (list "events"
           (mapcar (lambda (e)
                     (alexandria:plist-hash-table
                      (list "type" (string-downcase (symbol-name (rc:event-type e)))
                            "at" (rc:event-timestamp e)
                            "actor" (rc:event-actor e))
                      :test #'equal))
                   events))
     :test #'equal)))

(defun run-sub-view (run-id action)
  (cond
    ((string= action "candidates")
     (alexandria:plist-hash-table
      (list "candidates"
            (mapcar (lambda (c) (candidate-view (car c) nil))
                    (sort (loop :for id :being :the :hash-keys :of (ar:ar-candidates *runtime*)
                                :collect id)
                          #'string<)))
      :test #'equal))
    ((string= action "reviews")
     (alexandria:plist-hash-table (list "reviews" nil) :test #'equal))
    ((string= action "artifacts")
     (alexandria:plist-hash-table (list "artifacts" nil) :test #'equal))
    ((string= action "executions")
     (alexandria:plist-hash-table
      (list "executions"
            (mapcar (lambda (e)
                      (alexandria:plist-hash-table
                       (list "type" (string-downcase (symbol-name (rc:event-type e)))
                             "at" (rc:event-timestamp e))
                       :test #'equal))
                    (rc:replay-events (ar:ar-event-store *runtime*)
                                      :entity-id run-id
                                      :event-types '(:rlm.started :rlm.completed :rlm.failed
                                                     :tool.called :tool.succeeded :tool.failed))))
      :test #'equal))))

(defun candidate-view (candidate-id &optional reviews-p)
  (let ((candidate (gethash candidate-id (ar:ar-candidates *runtime*))))
    (unless candidate
      (error 'rc:validation-error
             :message (format nil "unknown candidate: ~a" candidate-id)))
    (alexandria:plist-hash-table
     (append
      (list "id" candidate-id
            "state" (string-downcase (symbol-name (cdr (assoc :state candidate))))
            "hypothesis" (cdr (assoc :hypothesis candidate))
            "run_id" (cdr (assoc :run-id candidate)))
      (when reviews-p
        (list "reviews" (or (mapcar (lambda (r)
                                      (alexandria:plist-hash-table
                                       (list "reviewer" (gethash "reviewer" r ""))
                                       :test #'equal))
                                    (cdr (assoc :reviews candidate)))
                            nil))))
     :test #'equal)))

(defun search-view (query project)
  (let ((results (ar:org-lexical-search (ar:ar-store *runtime*) query
                                        :project project)))
    (alexandria:plist-hash-table
     (list "results"
           (mapcar (lambda (r)
                     (alexandria:plist-hash-table
                      (list "path" (getf r :path)
                            "title" (getf r :title)
                            "score" (getf r :score))
                      :test #'equal))
                   results))
     :test #'equal)))

;;; --------------------------------------------------------------- SSE

(defun sse-body (run-id)
  "Render the DOMAIN event stream for RUN-ID as Server-Sent Events text.
Streams domain events, never raw model stdout."
  (with-output-to-string (out)
    (dolist (event (rc:replay-events (ar:ar-event-store *runtime*)
                                     :entity-id run-id))
      (format out "event: ~a~%data: ~a~%~%"
              (string-downcase (symbol-name (rc:event-type event)))
              (jz:stringify (alexandria:plist-hash-table
                             (list "entity" (rc:event-entity-id event)
                                   "at" (rc:event-timestamp event))
                             :test #'equal))))
    (format out "event: end~%data: {\"run\":~s}~%~%" run-id)))

(defun sse-events (run-id)
  (setf (hunchentoot:content-type*) "text/event-stream")
  (setf (hunchentoot:header-out :cache-control) "no-cache")
  (sse-body run-id))

;;; ------------------------------------------------------------ server

(defun start-api (&key (port 8080) (address "127.0.0.1"))
  "Start the /api/v1 acceptor. The runtime and principals must already be
configured by trusted init."
  (unless *runtime*
    (error 'rc:validation-error :message "runtime not configured"))
  (setf hunchentoot:*dispatch-table*
        (list (lambda (request)
                (when (ppcre:scan "^/api/v1/" (hunchentoot:script-name* request))
                  #'dispatch))))
  (setf *acceptor*
        (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
                                          :port port
                                          :address address)))
  *acceptor*)

(defun stop-api ()
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil)))
