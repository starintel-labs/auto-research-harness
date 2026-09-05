;;;; web-tests.lisp --- auth/capabilities, API surface, SSE, dashboard.

(in-package #:starintel-auto-research-tests)

(in-suite auto-research-suite)

(defparameter *web-port* 18432)

(defparameter *api-fixtures* nil)

(defun start-test-api ()
  (let* ((fixtures (make-test-corpus))
         (runtime (make-runtime-for (getf fixtures :root))))
    (setf *api-fixtures* fixtures)
    (web:clear-principals)
    (web:configure-principal
     "token-readonly"
     (rc:make-principal "reader" (list :|research:read|)))
    (web:configure-principal
     "token-operator"
     (rc:make-principal "operator"
                        (list :|research:read| :|research:submit|
                              :|research:cancel| :|research:approve|
                              :|runtime:inspect|)))
    (setf web:*runtime* runtime)
    (web:start-api :port *web-port* :address "127.0.0.1")))

(defun stop-test-api ()
  (ignore-errors (web:stop-api)))

(defun http (method path &optional body token)
  "HTTP helper that never signals on error statuses: returns (values body status)."
  (let ((dex (find-package :dex)))
    (handler-case
        (multiple-value-bind (response status)
            (funcall (find-symbol "REQUEST" dex)
                     (format nil "http://127.0.0.1:~a~a" *web-port* path)
                     :method (intern (string-upcase method) :keyword)
                     :headers (when token
                                (list (cons "Authorization" (format nil "Bearer ~a" token))))
                     :content (or body "")
                     :force-binary nil)
          (values (if (stringp response) response (princ-to-string response))
                  status))
      (error (condition)
        (let ((status (ignore-errors
                        (funcall (find-symbol "RESPONSE-STATUS" :dexador.error) condition))))
          (if (integerp status)
              (values (princ-to-string condition) status)
              (error condition)))))))

(defmacro with-test-api (&body body)
  `(progn
     (stop-test-api)
     (start-test-api)
     (unwind-protect (progn ,@body)
       (stop-test-api))))

;;; --------------------------------------------------------- auth layer

(test auth-fails-closed
  (with-test-api
    ;; missing token: authentication failure (401)
    (multiple-value-bind (response status)
        (http :get "/api/v1/projects")
      (declare (ignore response))
      (is (= 401 status)))
    ;; unknown token: authentication failure, not authorization (401)
    (multiple-value-bind (response status)
        (http :get "/api/v1/projects" nil "bogus-token")
      (declare (ignore response))
      (is (= 401 status)))
    ;; valid token without capability: authorization failure (403)
    (multiple-value-bind (response status)
        (http :post "/api/v1/projects/prolog-rlm/research"
              "{\"question\":\"q\"}" "token-readonly")
      (declare (ignore response))
      (is (= 403 status)))))

;;; ---------------------------------------------------------- API surface

(test api-projects-and-submit
  (with-test-api
    (multiple-value-bind (response status)
        (http :get "/api/v1/projects" nil "token-readonly")
      (is (= 200 status))
      (is (search "prolog-rlm" response)))
    (multiple-value-bind (response status)
        (http :post "/api/v1/projects/prolog-rlm/research"
              "{\"question\":\"Is direct mode bounded?\"}"
              "token-operator")
      (is (= 202 status))
      (is (search "\"run_id\"" response))
      (is (search "\"status\":\"accepted\"" response))
      ;; the submitted run is visible
      (let ((run-id (rlm.i:jstr (rlm.i:json-parse response) "run_id")))
        (multiple-value-bind (run-response run-status)
            (http :get (format nil "/api/v1/runs/~a" run-id) nil "token-readonly")
          (is (= 200 run-status))
          (is (search run-id run-response)))
        ;; events endpoint works
        (multiple-value-bind (events-response events-status)
            (http :get (format nil "/api/v1/runs/~a/events" run-id) nil "token-readonly")
          (declare (ignore events-response))
          (is (= 200 events-status)))
        ;; SSE streams domain events
        (multiple-value-bind (sse-response sse-status)
            (http :get (format nil "/api/v1/runs/~a/events/stream" run-id) nil "token-readonly")
          (is (= 200 sse-status))
          (is (search "event:" sse-response))))))
  ;; unknown project ids fail clearly with 404, never a silent default
  (with-test-api
    (multiple-value-bind (response status)
        (http :post "/api/v1/projects/star-lanng/research"
              "{\"question\":\"q\"}" "token-operator")
      (declare (ignore response))
      (is (= 404 status)))
    (multiple-value-bind (response status)
        (http :get "/api/v1/projects/star-lanng" nil "token-readonly")
      (declare (ignore response))
      (is (= 404 status)))))

(test api-submit-records-input-provenance
  ;; the resolved invocation shape (file vs text) is retained on the request
  (with-test-api
    (multiple-value-bind (response status)
        (http :post "/api/v1/projects/prolog-rlm/research"
              "{\"question\":\"q\",\"input_kind\":\"file\",\"input_path\":\"prompts/prolog-rlm/question.md\"}"
              "token-operator")
      (declare (ignore status))
      (let ((run-id (rlm.i:jstr (rlm.i:json-parse response) "run_id")))
        (let ((request-id (rc:request-id
                           (ar:ar-get-request
                            web:*runtime*
                            (rc:run-request-id (ar:ar-get-run web:*runtime* run-id))))))
          (multiple-value-bind (request-response request-status)
              (http :get (format nil "/api/v1/requests/~a" request-id)
                    nil "token-readonly")
            (is (= 200 request-status))
            (is (search "\"input_kind\":\"file\"" request-response))
            (is (search "prompts/prolog-rlm/question.md" request-response))))))))

(test api-requests-runs-cancel
  (with-test-api
    (multiple-value-bind (response status)
        (http :post "/api/v1/projects/auto-research/research"
              "{\"question\":\"Replay semantics\"}" "token-operator")
      (declare (ignore status))
      (let* ((parsed (rlm.i:json-parse response))
             (run-id (rlm.i:jstr parsed "run_id")))
        (multiple-value-bind (runs-response runs-status)
            (http :get "/api/v1/runs" nil "token-readonly")
          (is (= 200 runs-status))
          (is (search run-id runs-response)))
        (multiple-value-bind (cancel-response cancel-status)
            (http :post (format nil "/api/v1/runs/~a/cancel" run-id) "" "token-operator")
          (declare (ignore cancel-response))
          (is (= 200 cancel-status)))))))

(test api-artifacts-source-and-search
  (with-test-api
    (let* ((fixtures *api-fixtures*)
           (path (getf fixtures :path-1))
           (relative (enough-namestring path (getf fixtures :root))))
      (multiple-value-bind (meta-response meta-status)
          (http :get (format nil "/api/v1/artifacts/~a" relative) nil "token-readonly")
        (is (= 200 meta-status))
        (is (search "approval_state" meta-response)))
      (multiple-value-bind (source-response source-status)
          (http :get (format nil "/api/v1/artifacts/~a/source" relative) nil "token-readonly")
        (is (= 200 source-status))
        (is (search "#+title:" source-response)))
      (multiple-value-bind (search-response search-status)
          (http :get "/api/v1/search?q=direct" nil "token-readonly")
        (is (= 200 search-status))
        (is (search "results" search-response))))))

(test api-approval-requires-evidence-and-capability
  (with-test-api
    ;; reader principal cannot approve (403)
    (multiple-value-bind (response status)
        (http :post "/api/v1/artifacts/roam/research/prolog-rlm/direct-mode-research.org/approval"
              "{\"state\":\"APPROVED\",\"evidence\":\"x\"}" "token-readonly")
      (declare (ignore response))
      (is (= 403 status)))
    ;; operator without evidence is rejected (400)
    (multiple-value-bind (response status)
        (http :post "/api/v1/artifacts/roam/research/prolog-rlm/direct-mode-research.org/approval"
              "{\"state\":\"APPROVED\"}" "token-operator")
      (declare (ignore response))
      (is (= 400 status)))
    ;; valid approval persists with lifecycle untouched
    (multiple-value-bind (response status)
        (http :post "/api/v1/artifacts/roam/research/prolog-rlm/direct-mode-research.org/approval"
              "{\"state\":\"PENDING\",\"evidence\":\"reviewed by test operator\"}"
              "token-operator")
      (declare (ignore response))
      (is (= 200 status)))))

(test api-promotion-runs-guards
  (with-test-api
    ;; promotion of an unknown candidate is denied through the guards
    (multiple-value-bind (response status)
        (http :post "/api/v1/artifacts/roam/research/x.org/promote"
              "{\"candidate_id\":\"nope\"}" "token-operator")
      (declare (ignore response))
      (is (member status '(400 404 409))))))

(test api-run-candidates-view
  ;; regression: run-sub-view applied `car` to candidate id strings, so the
  ;; candidates endpoint returned 500 once any candidate existed
  (with-test-api
    (multiple-value-bind (request run)
        (ar:ar-submit-research web:*runtime*
                               (ar:find-project-config "prolog-rlm")
                               "Candidates view")
      (declare (ignore request))
      (let ((id (ar:ar-submit-candidate web:*runtime* (rc:run-id run)
                                        "p" "h" "pr" "f"
                                        nil :evidence "src/x.lisp")))
        (multiple-value-bind (response status)
            (http :get (format nil "/api/v1/runs/~a/candidates" (rc:run-id run))
                  nil "token-readonly")
          (is (= 200 status))
          (is (search id response)))))))

(test api-runtime-and-dashboard
  (with-test-api
    ;; runtime inspection requires runtime:inspect
    (multiple-value-bind (response status)
        (http :get "/api/v1/runtime" nil "token-readonly")
      (declare (ignore response))
      (is (= 403 status)))
    (multiple-value-bind (response status)
        (http :get "/api/v1/runtime" nil "token-operator")
      (is (= 200 status))
      (is (search "projects" response)))
    (multiple-value-bind (response status)
        (http :get "/api/v1/dashboard" nil "token-readonly")
      (declare (ignore response))
      (is (= 200 status))
      (is (search "runs" response)))
    (multiple-value-bind (health-response health-status)
        (http :get "/api/v1/health")
      (declare (ignore health-response))
      (is (= 200 health-status)))))

(test webhooks-are-verified-separately
  (with-test-api
    ;; unsigned payload is rejected even for a valid principal
    (multiple-value-bind (response status)
        (http :post "/api/v1/hooks/forgejo"
              "{\"instance\":\"https://git.example\",\"repository\":\"lost-rob0t/starintel-auto-research\",\"issue\":{\"number\":1,\"title\":\"t\",\"body\":\"b\"},\"action\":\"opened\",\"secret\":\"\"}"
              "token-operator")
      (declare (ignore response))
      (is (= 401 status)))))

(test api-run-pending-triggers-and-authorizes
  (with-test-api
    ;; no token: authentication failure
    (multiple-value-bind (response status)
        (http :post "/api/v1/run-pending" "")
      (declare (ignore response))
      (is (= 401 status)))
    ;; missing capability: authorization failure (readonly principal)
    (multiple-value-bind (response status)
        (http :post "/api/v1/run-pending" "{}" "token-readonly")
      (declare (ignore response))
      (is (= 403 status)))
    ;; authorized: 202 accepted and a run advances past :queued
    (multiple-value-bind (request run)
        (ar:ar-submit-research web:*runtime*
                               (ar:find-project-config "prolog-rlm")
                               "Endpoint-triggered walk")
      (declare (ignore request))
      (multiple-value-bind (response status)
          (http :post "/api/v1/run-pending" "{}" "token-operator")
        (is (= 202 status))
        (is (search "started" response))
        ;; the worker thread walks stages without a provider; the run must
        ;; leave :queued within a few seconds (fail-closed :unresolved)
        (let ((deadline (+ (get-internal-real-time)
                           (* 10 internal-time-units-per-second))))
          (loop :while (and (eq :queued (rc:run-status run))
                            (< (get-internal-real-time) deadline))
                :do (sleep 0.2))
          (is (member (rc:run-status run)
                      '(:running :completed :unresolved :failed))))))))
