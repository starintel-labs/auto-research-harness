;;;; rlm-tests.lisp --- harness, tools, transport, cancellation.

(in-package #:starintel-auto-research-tests)

(in-suite auto-research-suite)

;;; ---------------------------------------------------- tool definitions

(test tool-registration-and-schema
  (let ((registry (rlm:make-registry)))
    (rlm:register-tool registry (rlm:make-tool
                                 :name "search-research"
                                 :description "search"
                                 :parameters '(("type" . "object"))
                                 :handler (lambda (args) args)))
    (is-true (rlm:find-tool registry "search-research"))
    (is-false (rlm:find-tool registry "missing"))
    (let ((tool (rlm:find-tool registry "search-research")))
      (let ((wire (rlm:tool-wire-schema tool)))
        (is (string= "function" (rlm.i:jstr wire "type")))
        (is (string= "search-research"
                     (rlm.i:jstr (rlm.i:jref wire "function") "name")))))
    ;; duplicate registration is rejected
    (signals error
      (rlm:register-tool registry (rlm:make-tool
                                   :name "search-research"
                                   :description "dup"
                                   :handler (lambda (a) a))))))

(test tool-argument-validation
  (let ((registry (rlm:make-registry)))
    (rlm:register-tool
     registry
     (rlm:make-tool :name "record-finding"
                    :description "record"
                    :parameters '(("type" . "object")
                                  ("properties" . (("path" . (("type" . "string")))
                                                   ("finding" . (("type" . "string")))))
                                  ("required" . ("path" "finding")))
                    :handler (lambda (args) args)))
    ;; missing required field fails before the handler runs
    (signals rlm:tool-argument-error
      (rlm:invoke-registry-tool registry "record-finding" (make-hash-table :test #'equal)))
    ;; wrong type fails
    (signals rlm:tool-argument-error
      (rlm:invoke-registry-tool
       registry "record-finding"
       (alexandria:plist-hash-table (list "path" 1 "finding" "x") :test #'equal)))
    ;; valid arguments pass validation and reach the handler
    (multiple-value-bind (result audit)
        (rlm:invoke-registry-tool
         registry "record-finding"
         (alexandria:plist-hash-table (list "path" "p" "finding" "x") :test #'equal))
      (declare (ignore result))
      (is (eq :succeeded (getf audit :status))))
    ;; unknown tool is an error
    (signals error (rlm:invoke-registry-tool registry "nope" (make-hash-table :test #'equal)))))

(test tool-output-cap
  (let ((registry (rlm:make-registry)))
    (rlm:register-tool
     registry
     (rlm:make-tool :name "big"
                    :description "big output"
                    :max-output-bytes 16
                    :handler (lambda (args) (declare (ignore args))
                               (make-string 40 :initial-element #\x))))
    (signals rlm:tool-output-error
      (rlm:invoke-registry-tool registry "big" (make-hash-table :test #'equal)))))

(test make-context-bounds
  (let ((context (rlm:make-context (make-string 100 :initial-element #\a)
                                   :max-bytes 32)))
    (is (= 32 (rlm:context-bytes context)))
    (is (= 32 (length (rlm:context-text context))))
    (is (>= (length (babel:string-to-octets (rlm:context-text context)))
            32))))

;;; --------------------------------------------------- scripted transport

(test fake-transport-run-completes
  (let* ((registry (rlm:make-registry))
         (run (rlm:run :query "Say FAKE_OK"
                       :registry registry
                       :transport (fake-transport))))
    (is (eq :succeeded (rlm:run-status run)))
    (is (string= "Say FAKE_OK" (rlm:run-result run)))
    (is (equal 15 (getf (rlm:run-usage run) :total-tokens)))
    (is (= 0 (length (rlm:run-tool-calls run))))))

(test fake-transport-tool-roundtrip
  ;; the fake bridge calls search-research once; the runtime semantic tool
  ;; must round-trip and return corpus results
  (let* ((fixtures (make-test-corpus))
         (store (getf fixtures :store))
         (registry (ar:make-research-tool-registry)))
    (ar:install-research-tools registry
                               ;; minimal runtime stand-in
                               (make-instance 'ar:ar-runtime
                                              :root (getf fixtures :root)
                                              :store store
                                              :rlm-registry registry))
    (let* ((run (rlm:run :query "Search the corpus"
                         :registry registry
                         :transport (fake-transport))))
      (is (eq :succeeded (rlm:run-status run)))
      (let ((call (first (rlm:run-tool-calls run))))
        (is (eq :succeeded (getf call :status)))
        (is (string= "search-research" (getf call :tool)))))))

(test run-failure-captured-structurally
  ;; dead endpoint -> structured transport failure, never an unhandled error
  (let* ((provider (rlm:make-provider :endpoint "http://127.0.0.1:9/v1/chat/completions"
                                      :model "test-model"
                                      :no-credential t))
         (run (rlm:run :query "Say OK"
                       :provider provider)))
    (unless (eq (rlm:run-status run) :succeeded)
      (is-true (getf (rlm:run-error run) :message)))
    (is (member (rlm:run-status run) '(:succeeded :failed)))))

(test cancellation
  ;; a hung bridge (never answers) must be cancellable and end cancelled
  (let ((bridge (merge-pathnames "hang_bridge.pl"
                                 (asdf:system-source-directory
                                  :starintel-auto-research-tests))))
    (ensure-hang-bridge bridge)
    (let* ((transport (rlm:make-subprocess-transport
                       :swipl (find-prolog-rlm-swipl)
                       :bridge (namestring (truename bridge))))
           (run (rlm:run-async :query "hang"
                               :transport transport)))
      (is (member (rlm:run-status run) '(:pending :running)))
      (is-true (rlm:cancel run))
      (is (eq :cancelled (rlm:run-status run))))))

(defun ensure-hang-bridge (path)
  (unless (probe-file path)
    (with-open-file (out path :direction :output)
      (format out ":- initialization(main, main).~%")
      (format out ":- use_module(library(http/json)).~%")
      (format out "main :- json_read_dict(current_input, _, [end_of_file(error)]), sleep(3600), halt(0).~%"))))
