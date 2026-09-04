;;;; tools.lisp --- Auto Research semantic tools.
;;;;
;;;; The model receives a narrow semantic tool set. CL owns canonical Org
;;;; serialization: record-source/record-finding/etc. take structured
;;;; values and CL produces validator-clean Org. Generic low-level file
;;;; tools exist in the generic harness (root-scoped) but are NOT part of
;;;; this interface. Every tool declares its effect class, and CL validates
;;;; arguments before handlers run.

(in-package #:starintel-auto-research)

(defun schema-key (name)
  (cond
    ((keywordp name) (string-downcase (symbol-name name)))
    ((symbolp name) (string-downcase (symbol-name name)))
    (t name)))

(defun jschema (properties &optional required)
  "JSON-schema object: PROPERTIES is a flat plist of name type-string
pairs; REQUIRED is a list of property names."
  (let ((props (make-hash-table :test #'equal)))
    (loop :for (name type) :on properties :by #'cddr
          :when (and name type)
            :do (setf (gethash (schema-key name) props)
                      (alexandria:plist-hash-table (list "type" type) :test #'equal)))
    (let ((schema (make-hash-table :test #'equal)))
      (setf (gethash "type" schema) "object"
            (gethash "properties" schema) props)
      (when required
        (setf (gethash "required" schema)
              (mapcar #'schema-key required)))
      schema)))

(defun require-string-field (args field)
  (let ((value (gethash field args)))
    (unless (and (stringp value) (plusp (length value)))
      (error 'research-core:validation-error
             :message (format nil "tool argument ~a must be a non-empty string" field)))
    value))

(defun require-slug-field (args field)
  (let ((value (require-string-field args field)))
    (unless (ppcre:scan "^[a-z0-9][a-z0-9-]*$" value)
      (error 'research-core:validation-error
             :message (format nil "tool argument ~a must be a slug: ~a" field value)))
    value))

(defun state-field (args field allowed)
  (let ((value (require-string-field args field)))
    (unless (member value allowed :test #'string=)
      (error 'research-core:validation-error
             :message (format nil "tool argument ~a must be one of {~{~a~^ ~}}"
                              field allowed)))
    value))

(defun make-research-tool-registry ()
  "A fresh registry holding ONLY the semantic tool set."
  (rlm:make-registry))

(defun register-semantic-tool (registry runtime name description properties
                               required effect handler)
  (rlm:register-tool
   registry
   (rlm:make-tool
    :name name
    :description description
    :parameters (jschema properties required)
    :effect effect
    :side-effect (if (eq effect :read) :none :data-write)
    :time-limit 2.0d0
    :max-output-bytes 16384
    :handler handler)))

(defun install-research-tools (registry runtime)
  "Register the Auto Research semantic tools on REGISTRY, bound to
RUNTIME. Returns the registry."
  (register-semantic-tool
   registry runtime "search-research"
   "Lexically search the canonical research corpus. Returns scored results with path, title, and description."
   (list "query" "string") (list "query")
   :read
   (lambda (args)
     (org-lexical-search (ar-store runtime)
                         (require-string-field args "query")
                         :project (gethash "project" args)
                         :limit (or (gethash "limit" args) 12))))
  (register-semantic-tool
   registry runtime "read-research-node"
   "Read one research node by relative path. Returns parsed metadata, approval state, and lifecycle."
   (list "path" "string") (list "path")
   :read
   (lambda (args)
     (multiple-value-bind (text keywords approval lifecycle)
         (read-research-node
          (ensure-path-in-store%/ (ar-store runtime)
                                  (require-string-field args "path")))
       (sar.i:dict "approval_state" (string-downcase (symbol-name approval))
                   "lifecycle" (string-downcase (symbol-name lifecycle))
                   "title" (gethash "title" keywords "")
                   "status" (gethash "status" keywords "")
                   "description" (gethash "description" keywords "")
                   "excerpt" (excerpt text 4000)))))
  (register-semantic-tool
   registry runtime "read-design-node"
   "Read one design node by relative path. Returns parsed metadata and excerpt."
   (list "path" "string") (list "path")
   :read
   (lambda (args)
     (let ((text (uiop:read-file-string
                  (ensure-path-in-store%/ (ar-store runtime)
                                          (require-string-field args "path")))))
       (let ((keywords (parse-org-keywords text)))
         (sar.i:dict "title" (gethash "title" keywords "")
                     "status" (gethash "status" keywords "")
                     "excerpt" (excerpt text 4000))))))
  (register-semantic-tool
   registry runtime "repository-read"
   "Read one UTF-8 file beneath the configured research repository root. Rejects traversal."
   (list "path" "string") (list "path")
   :read
   (lambda (args)
     (uiop:read-file-string
      (ensure-path-in-store%/ (ar-store runtime)
                              (require-string-field args "path")))))
  ;; semantic write tools ----------------------------------------------
  (register-semantic-tool
   registry runtime "record-source"
   "Record one retrieved source (URL or citation text) with its retrieval date on a research node."
   (list "path" "string" "source" "string") (list "path" "source")
   :write
   (lambda (args)
     (append-research-update
      (ensure-path-in-store%/ (ar-store runtime)
                              (require-string-field args "path"))
      :sources (list (require-string-field args "source")))))
  (register-semantic-tool
   registry runtime "record-finding"
   "Record one bounded finding on a research node."
   (list "path" "string" "finding" "string") (list "path" "finding")
   :write
   (lambda (args)
     (append-research-update
      (ensure-path-in-store%/ (ar-store runtime)
                              (require-string-field args "path"))
      :findings (require-string-field args "finding"))))
  (register-semantic-tool
   registry runtime "record-counterevidence"
   "Record contrary evidence on a research node. Every material claim needs explicit contrary evidence."
   (list "path" "string" "evidence" "string") (list "path" "evidence")
   :write
   (lambda (args)
     (append-research-update
      (ensure-path-in-store%/ (ar-store runtime)
                              (require-string-field args "path"))
      :findings (format nil "Counterevidence: ~a"
                        (require-string-field args "evidence")))))
  (register-semantic-tool
   registry runtime "record-falsifier"
   "Record the falsifier: the observable result that would reject the hypothesis."
   (list "path" "string" "falsifier" "string") (list "path" "falsifier")
   :write
   (lambda (args)
     (append-research-update
      (ensure-path-in-store%/ (ar-store runtime)
                              (require-string-field args "path"))
      :findings (format nil "Falsifier: ~a"
                        (require-string-field args "falsifier")))))
  (register-semantic-tool
   registry runtime "record-unresolved-question"
   "Record one explicitly unresolved question on a research node."
   (list "path" "string" "question" "string") (list "path" "question")
   :write
   (lambda (args)
     (append-research-update
      (ensure-path-in-store%/ (ar-store runtime)
                              (require-string-field args "path"))
      :findings (format nil "Unresolved question: ~a"
                        (require-string-field args "question")))))
  (register-semantic-tool
   registry runtime "record-research-pass"
   "Record one completed research pass (findings plus optional sources/commits) on a research node."
   (list "path" "string" "finding" "string" "source" "string" "commit" "string")
   (list "path" "finding")
   :write
   (lambda (args)
     (append-research-update
      (ensure-path-in-store%/ (ar-store runtime)
                              (require-string-field args "path"))
      :findings (require-string-field args "finding")
      :sources (when (gethash "source" args)
                 (list (gethash "source" args)))
      :commits (when (gethash "commit" args)
                 (list (gethash "commit" args))))))
  (register-semantic-tool
   registry runtime "submit-candidate"
   "Submit a bounded research candidate with hypothesis, predicted result, and falsifier. Returns the candidate id."
   (list "run_id" "string" "hypothesis" "string" "predicted_result" "string"
         "falsifier" "string" "problem" "string")
   (list "run_id" "hypothesis" "predicted_result" "falsifier" "problem")
   :write
   (lambda (args)
     (let ((id (ar-submit-candidate
                runtime
                (require-string-field args "run_id")
                (require-string-field args "problem")
                (require-string-field args "hypothesis")
                (require-string-field args "predicted_result")
                (require-string-field args "falsifier")
                (gethash "parent_ids" args))))
       (sar.i:dict "candidate_id" id))))
  (register-semantic-tool
   registry runtime "record-review"
   "Record one independent review result for a candidate (scores and optional veto reason)."
   (list "candidate_id" "string" "reviewer" "string" "evidence" "number"
         "architecture" "number" "novelty" "number" "feasibility" "number"
         "testability" "number" "leverage" "number" "scope" "number"
         "veto" "string")
   (list "candidate_id")
   :write
   (lambda (args)
     (let ((candidate-id (require-string-field args "candidate_id")))
       (ar-record-review runtime candidate-id args)
       (sar.i:dict "recorded" candidate_id))))
  registry)

(defun ensure-path-in-store (runtime path)
  "Resolve PATH beneath the configured corpus root; reject ../ escapes,
unauthorized absolute paths, and symlink escapes."
  (declare (ignore runtime))
  (let* ((root (namestring (uiop:getcwd)))
         (full (if (uiop:absolute-pathname-p path)
                   (probe-file path)
                   (probe-file (merge-pathnames path root)))))
    (unless (and full (uiop:file-exists-p full)
                 (uiop:subpathp full (pathname root)))
      (error 'research-core:validation-error
             :message (format nil "path is outside the configured root: ~a" path)))
    (when (uiop:pathname-equal (truename full) full)
      nil)
    (unless (equal (truename full) full)
      (error 'research-core:validation-error
             :message (format nil "symlink escape rejected: ~a" path)))
    full))

(defun excerpt (text limit)
  (if (<= (length text) limit)
      text
      (concatenate 'string (subseq text 0 limit) "…")))

;;; The store-root variant used by the runtime tools
(defun ensure-path-in-store%/ (store path)
  (let* ((root (org-roam-root store))
         (full (merge-pathnames path root)))
    (unless (uiop:subpathp full (pathname root))
      (error 'research-core:validation-error :message "path escapes corpus"))
    (unless (probe-file full)
      (error 'research-core:validation-error
             :message (format nil "missing artifact: ~a" path)))
    (unless (equal (truename full) (truename root))
      (unless (uiop:subpathp (truename full) (truename (pathname root)))
        (error 'research-core:validation-error
               :message (format nil "symlink escape rejected: ~a" path))))
    full))
