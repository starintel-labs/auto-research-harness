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

(defun optional-string-arg (args field)
  "Absent-tolerant string argument: JSON null or a missing field is nil;
anything else must be a non-empty string."
  (let ((value (gethash field args)))
    (cond
      ((null value) nil)
      ((stringp value)
       (if (plusp (length value)) value nil))
      (t (error 'research-core:validation-error
                :message (format nil "tool argument ~a must be a string" field))))))

(defun ar-run-project-slug (runtime run-id)
  "Resolve the roam project slug for RUN-ID through its request."
  (let* ((run (ar-get-run runtime run-id))
         (request (when run (ar-get-request runtime
                                            (research-core:run-request-id run)))))
    (when request
      (project-config-roam-slug
       (find-project-config
        (project-config-id (research-core:request-project request)))))))

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
    :max-output-bytes 6144
    :handler (lambda (args)
               ;; A session dies when a tool handler raises, so validation
               ;; errors are returned as model-readable values instead; the
               ;; model can correct its arguments and retry.
               (handler-case (funcall handler args)
                 (research-core:validation-error (condition)
                   (sar.i:dict "error" (format nil "~a" condition))))))))

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
                          :limit (or (gethash "limit" args) 5))))
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
                   "excerpt" (excerpt text 1200)))))
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
                     "excerpt" (excerpt text 1200))))))
  (register-semantic-tool
   registry runtime "repository-read"
   "Read one UTF-8 file beneath the ACTIVE RUN's configured source repository root (the repository being researched), never the research corpus. Rejects traversal."
   (list "path" "string") (list "path")
   :read
   (lambda (args)
     (let* ((path (ensure-repository-path runtime (require-string-field args "path")))
            (text (uiop:read-file-string path)))
       (note-repository-observation runtime path "repository-read")
       (excerpt text 4096))))
  (register-semantic-tool
   registry runtime "repository-list"
   "List files beneath a directory of the ACTIVE RUN's configured source repository. Returns bounded relative paths; never reads the research corpus."
   (list "path" "string" "limit" "number") '()
   :read
   (lambda (args)
     (let* ((limit (min (or (gethash "limit" args) 200) 1000))
            (root-path (ensure-repository-path runtime (or (gethash "path" args) "")))
            (root (pathname (current-repository-root runtime)))
            (files))
       (labels ((git-dir-p (dir)
                  (search ".git" (namestring dir)))
                (walk (dir)
                  (unless (git-dir-p dir)
                    (dolist (file (uiop:directory-files dir))
                      (when (< (length files) limit)
                        (push file files)))
                    (dolist (sub (uiop:subdirectories dir))
                      (when (< (length files) limit)
                        (walk sub))))))
         (walk root-path))
       (note-repository-observation runtime root-path "repository-list")
       (sar.i:dict
        "repository" (or (repository-label runtime) "unknown")
        "files" (mapcar (lambda (file)
                          (enough-namestring file root))
                        (nreverse files))
        "truncated" (>= (length files) limit)))))
  (register-semantic-tool
   registry runtime "repository-search"
   "Literal text search across Lisp files of the ACTIVE RUN's configured source repository. Returns bounded path:line matches; never reads the research corpus."
   (list "query" "string" "limit" "number") (list "query")
   :read
   (lambda (args)
     (let* ((query (require-string-field args "query"))
            (limit (min (or (gethash "limit" args) 20) 100))
            (root-path (ensure-repository-path runtime ""))
            (root (pathname (current-repository-root runtime)))
            (matches)
            (scanned 0))
       (flet ((search-file (file)
                (incf scanned)
                (let ((text (ignore-errors (uiop:read-file-string file))))
                  (when (and text (< (length matches) limit))
                    (let ((lines (ppcre:split "\\n" text)))
                      (loop :for line :in lines
                            :for n :from 1
                            :when (and (search query line :test #'char-equal)
                                       (< (length matches) limit))
                              :do (push (format nil "~a:~a: ~a"
                                                (enough-namestring file root)
                                                n
                                                (excerpt (string-trim " " line) 160))
                                        matches)
                              :and :do (return)))))))
         (labels ((git-dir-p (dir)
                    (search ".git" (namestring dir)))
                  (walk (dir)
                    (unless (git-dir-p dir)
                      (dolist (file (uiop:directory-files dir "*.lisp"))
                        (search-file file))
                      (dolist (sub (uiop:subdirectories dir))
                        (walk sub)))))
           (walk root-path)))
       (note-repository-observation runtime root-path "repository-search")
       (sar.i:dict
        "repository" (or (repository-label runtime) "unknown")
        "matches" (nreverse matches)
        "files-scanned" scanned))))
  (register-semantic-tool
   registry runtime "repository-info"
   "Read-only git metadata (HEAD and recent commits) of the ACTIVE RUN's configured source repository, when supported. Never reads the research corpus."
   (list) '()
   :read
   (lambda (args)
     (declare (ignore args))
     (let* ((repo-root (ensure-repository-path runtime ""))
            (root (current-repository-root runtime)))
       (note-repository-observation runtime repo-root "repository-info")
       (labels ((git (arguments)
                  (nth-value 0
                             (uiop:run-program (append (list "git" "-C" root) arguments)
                                               :output :string :error-output :string
                                               :ignore-error-status t))))
         (sar.i:dict
          "repository" (or (repository-label runtime) "unknown")
          "head" (string-trim " \n" (git (list "rev-parse" "HEAD")))
          "recent-commits" (string-trim " \n" (git (list "log" "--oneline" "-5"))))))))
  ;; semantic write tools ----------------------------------------------
  (flet ((note-artifact (path)
           (ar-note-research-artifact runtime *ar-current-run-id* path)))
    (register-semantic-tool
     registry runtime "record-source"
     "Record one retrieved source (URL or citation text) with its retrieval date on a research node."
     (list "path" "string" "source" "string") (list "path" "source")
     :write
     (lambda (args)
       (let* ((path (ensure-path-in-store%/ (ar-store runtime)
                                            (require-string-field args "path")))
              (source (require-string-field args "source")))
         (append-research-update path :sources (list source))
         (note-artifact path)
         (ar-note-evidence runtime *ar-current-run-id* :source path source)
         (sar.i:dict "recorded" (enough-namestring path (org-roam-root (ar-store runtime)))))))
    (register-semantic-tool
     registry runtime "record-finding"
     "Record one bounded finding on a research node."
     (list "path" "string" "finding" "string") (list "path" "finding")
     :write
     (lambda (args)
       (let* ((path (ensure-path-in-store%/ (ar-store runtime)
                                            (require-string-field args "path")))
              (finding (require-string-field args "finding")))
         (append-research-update path :findings finding)
         (note-artifact path)
         (ar-note-evidence runtime *ar-current-run-id* :finding path finding)
         (sar.i:dict "recorded" (enough-namestring path (org-roam-root (ar-store runtime)))))))
    (register-semantic-tool
     registry runtime "record-counterevidence"
     "Record contrary evidence on a research node. Every material claim needs explicit contrary evidence."
     (list "path" "string" "evidence" "string") (list "path" "evidence")
     :write
     (lambda (args)
       (let* ((path (ensure-path-in-store%/ (ar-store runtime)
                                            (require-string-field args "path")))
              (evidence (require-string-field args "evidence")))
         (append-research-update
          path
          :findings (format nil "Counterevidence: ~a" evidence))
         (note-artifact path)
         (ar-note-evidence runtime *ar-current-run-id* :counterevidence path evidence)
         (sar.i:dict "recorded" (enough-namestring path (org-roam-root (ar-store runtime)))))))
    (register-semantic-tool
     registry runtime "record-falsifier"
     "Record the falsifier: the observable result that would reject the hypothesis."
     (list "path" "string" "falsifier" "string") (list "path" "falsifier")
     :write
     (lambda (args)
       (let* ((path (ensure-path-in-store%/ (ar-store runtime)
                                            (require-string-field args "path")))
              (falsifier (require-string-field args "falsifier")))
         (append-research-update
          path
          :findings (format nil "Falsifier: ~a" falsifier))
         (note-artifact path)
         (ar-note-evidence runtime *ar-current-run-id* :finding path
                           (format nil "Falsifier: ~a" falsifier))
         (sar.i:dict "recorded" (enough-namestring path (org-roam-root (ar-store runtime)))))))
    (register-semantic-tool
     registry runtime "record-unresolved-question"
     "Record one explicitly unresolved question on a research node."
     (list "path" "string" "question" "string") (list "path" "question")
     :write
     (lambda (args)
       (let* ((path (ensure-path-in-store%/ (ar-store runtime)
                                            (require-string-field args "path")))
              (question (require-string-field args "question")))
         (append-research-update
          path
          :findings (format nil "Unresolved question: ~a" question))
         (note-artifact path)
         (ar-note-evidence runtime *ar-current-run-id* :unresolved-question path question)
         (sar.i:dict "recorded" (enough-namestring path (org-roam-root (ar-store runtime)))))))
    (register-semantic-tool
     registry runtime "record-research-pass"
     "Record one completed research pass (findings plus optional sources/commits) on a research node."
     (list "path" "string" "finding" "string" "source" "string" "commit" "string")
     (list "path" "finding")
     :write
     (lambda (args)
       (let* ((path (ensure-path-in-store%/ (ar-store runtime)
                                            (require-string-field args "path")))
              (finding (require-string-field args "finding"))
              (source (and (gethash "source" args) (gethash "source" args))))
         (append-research-update
          path
          :findings finding
          :sources (when source (list source))
          :commits (when (gethash "commit" args)
                     (list (gethash "commit" args))))
          (note-artifact path)
          (ar-note-evidence runtime *ar-current-run-id* :finding path finding)
          (when source
            (ar-note-evidence runtime *ar-current-run-id* :source path source))
          (sar.i:dict "recorded" (enough-namestring path (org-roam-root (ar-store runtime))))))))
  (register-semantic-tool
   registry runtime "submit-candidate"
   "Submit a bounded research candidate with hypothesis, predicted result, and falsifier. Optional: one primary evidence citation, one acceptance criterion, and one rejected alternative. Returns the candidate id."
   (list "run_id" "string" "hypothesis" "string" "predicted_result" "string"
         "falsifier" "string" "problem" "string" "evidence" "string"
         "acceptance_criteria" "string" "rejected_alternatives" "string")
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
                (gethash "parent_ids" args)
                :evidence (optional-string-arg args "evidence")
                :acceptance-criteria (optional-string-arg args "acceptance_criteria")
                :rejected-alternatives (optional-string-arg args "rejected_alternatives"))))
       (sar.i:dict "candidate_id" id))))
  (register-semantic-tool
   registry runtime "record-design-note"
   "Record the bounded design note for this run as a canonical design node. CL serializes the design plus the canonical Research Basis / Evidence Used provenance sections derived from the run evidence bundle; the model supplies structured text only."
   (list "run_id" "string" "design" "string") (list "run_id" "design")
   :write
   (lambda (args)
     (let* ((run-id (require-string-field args "run_id"))
            (slug (or (ar-run-project-slug runtime run-id)
                      (error 'research-core:validation-error
                             :message (format nil "unknown run: ~a" run-id))))
            (run (or (ar-get-run runtime run-id)
                     (error 'research-core:validation-error
                            :message (format nil "unknown run: ~a" run-id))))
            (bundle (run-evidence-bundle runtime run)))
       (multiple-value-bind (path)
           (write-design-node
            (ar-store runtime) slug run-id
            (format nil "Design ~a" run-id)
            (require-string-field args "design")
            :research-basis (design-basis-items runtime bundle)
            :evidence-rows (design-evidence-rows bundle))
         (sar.i:dict "path" (enough-namestring path (org-roam-root (ar-store runtime))))))))
  (register-semantic-tool
   registry runtime "record-design-review"
   "Record one bounded design review with an explicit verdict on this run's design note."
   (list "run_id" "string" "review" "string" "verdict" "string")
   (list "run_id" "review" "verdict")
   :write
   (lambda (args)
     (let* ((run-id (require-string-field args "run_id"))
            (slug (or (ar-run-project-slug runtime run-id)
                      (error 'research-core:validation-error
                             :message (format nil "unknown run: ~a" run-id))))
            (path (design-node-path (ar-store runtime) slug run-id)))
       (unless (probe-file path)
         (error 'research-core:validation-error
                :message (format nil "no design note for run: ~a" run-id)))
       (append-design-review path
                             (require-string-field args "review")
                             (require-string-field args "verdict"))
       (sar.i:dict "path" (enough-namestring path (org-roam-root (ar-store runtime)))))))
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
       (sar.i:dict "recorded" candidate-id))))
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

;;; ------------------------------------------------- source repository tools
;;;
;;; The research corpus (org-roam store) and the SOURCE repository being
;;; researched are distinct. The repository-* tools expose safe, read-only
;;; operations against the SOURCE repository configured on the active
;;; run's project; they never read or write the corpus, and they never
;;; grant write access.

(defun current-run-project (runtime)
  "The project of the run currently executing a semantic stage, or nil
outside a run."
  (when *ar-current-run-id*
    (let ((run (ar-get-run runtime *ar-current-run-id*)))
      (when run
        (let ((request (ar-get-request runtime
                                       (research-core:run-request-id run))))
          (when request
            (find-project-config
             (project-config-id
              (research-core:request-project request)))))))))

(defun ensure-repository-path (runtime path)
  "Resolve PATH beneath the ACTIVE RUN's configured source repository
root; reject traversal and symlink escapes. Errors when the run's
project configures no source repository: repository tools must never be
confused with corpus reads."
  (let* ((project (current-run-project runtime))
         (root (and project (project-config-source-path project))))
    (unless root
      (error 'research-core:validation-error
             :message (format nil
                              "repository tools require a source repository; project ~a configures none"
                              (if project (project-config-id project) "<no run>"))))
    (let* ((full (merge-pathnames (or path "") (pathname root)))
           (resolved (probe-file full)))
      (unless (and resolved (uiop:subpathp resolved (pathname root)))
        (error 'research-core:validation-error
               :message (format nil "path is outside the configured source repository: ~a" path)))
      (unless (uiop:subpathp (truename resolved) (truename (pathname root)))
        (error 'research-core:validation-error
               :message (format nil "symlink escape rejected: ~a" path)))
      resolved)))

(defun repository-label (runtime)
  "Provenance label of the source repository the repository tools read."
  (let ((project (current-run-project runtime)))
    (and project (project-config-repository project))))

(defun note-repository-observation (runtime path tool)
  (ar-note-evidence runtime *ar-current-run-id* :repository-observation
                    nil
                    (format nil "~a ~a [~a]"
                            tool
                            (enough-namestring path (current-repository-root runtime))
                            (or (repository-label runtime) "unknown"))))

(defun current-repository-root (runtime)
  (let ((project (current-run-project runtime)))
    (and project (project-config-source-path project))))
