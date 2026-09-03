;;;; org-roam-store.lisp --- the Org-roam artifact store.
;;;;
;;;; Implements the artifact-store protocol over the canonical layout:
;;;;   roam/research/<project>/, roam/design/<project>/,
;;;;   roam/implement/<project>/, roam/indexes/<project>/,
;;;;   roam/research/ardr-issues/
;;;; The mirrored subtree name is the project identity. Generated files
;;;; must pass the CURRENT canonical validators (scripts/validate-docs.py).

(in-package #:starintel-auto-research)

(defclass org-roam-store (research-core:artifact-store)
  ((root :initarg :root :reader org-roam-root)
   (base-commit :initarg :base-commit :initform "uncommitted"
                :reader org-roam-base-commit)))

(defun make-org-roam-store (root &key (base-commit "uncommitted"))
  (make-instance 'org-roam-store
                 :root (namestring (truename root))
                 :base-commit base-commit))

(defparameter +roam-trees+ '("design" "research" "implement" "indexes")
  "Canonical roam trees. The mirrored subtree name is project identity.")

(defun ensure-roam-layout (root)
  "Create roam/ with the canonical trees and ledgers, then mirror the
visible directory structure across all trees (port of _roamlib)."
  (let ((roam (merge-pathnames "roam/" root)))
    (ensure-directories-exist roam)
    (dolist (tree +roam-trees+)
      (ensure-directories-exist (merge-pathnames (concatenate 'string tree "/") roam)))
    (dolist (ledger '(".implemented" ".rejected"))
      (unless (probe-file (merge-pathnames ledger roam))
        (with-open-file (out (merge-pathnames ledger roam) :direction :output)
          (declare (ignore out)))))
    (mirror-roam-structure roam)
    roam))

(defun visible-rel-dirs (tree-dir)
  (let ((dirs))
    (when (probe-file tree-dir)
      (labels ((walk (dir relative)
                 (dolist (entry (uiop:subdirectories dir))
                   (let* ((name (car (last (pathname-directory entry))))
                          (rel (append relative (list name))))
                     (unless (char= (char name 0) #\.)
                       (push rel dirs)
                       (walk entry rel))))))
        (walk tree-dir nil)))
    dirs))

(defun mirror-roam-structure (roam)
  "Every visible relative directory under any tree exists under every tree."
  (let ((rels (mapcar #'nreverse
                      (loop :for tree :in +roam-trees+
                            :append (visible-rel-dirs
                                     (merge-pathnames (concatenate 'string tree "/") roam))))))
    (dolist (rel rels)
      (dolist (tree +roam-trees+)
        (ensure-directories-exist
         (make-pathname :directory (append (pathname-directory roam)
                                           (list tree)
                                           rel)))))))

(defun roam-project-dir (store tree slug)
  (ensure-directories-exist
   (merge-pathnames (format nil "roam/~a/~a/" tree slug)
                    (org-roam-root store))))

(defun stable-file-id (relative-path)
  (format nil "starintel-~a"
          (subseq (ironclad:byte-array-to-hex-string
                   (ironclad:digest-sequence :sha256 relative-path))
                  0 32)))

(defun canonical-research-template (id title description filetags status)
  "Canonical research node skeleton matching scripts/save-research.py output:
stable :ID:, required keywords, objective and findings sections."
  (format nil ":PROPERTIES:
:ID:       ~a
:END:
#+title: ~a
#+description: ~a
#+status: ~a
#+filetags: ~a
#+todo: DRAFT RESEARCH REVIEW | DONE REJECTED

* ~a Objective

~a

* ~a Findings
" id title description filetags status status description status))

;;; ------------------------------------------------- research write paths

(defun write-research-node (store slug title &key (description "") (state :draft))
  "Create (or refuse to overwrite) a canonical research node for SLUG.
Returns the absolute path. CL owns all Org formatting; the model never
writes Org directly."
  (let* ((dir (roam-project-dir store "research" slug))
         (path (merge-pathnames (format nil "~a.org" (sar.i:slugify title)) dir)))
    (when (probe-file path)
      (error 'research-core:validation-error
             :message (format nil "refusing overwrite: ~a" path)))
    (let* ((relative (enough-namestring path (org-roam-root store)))
           (id (stable-file-id relative))
           (status (ecase state (:draft "DRAFT") (:final "DONE")))
           (tags (format nil ":starintel:research:~a:~a:" slug
                         (ecase state (:draft "draft") (:final ""))))
           (text (canonical-research-template id title description
                                              (if (string= tags ":starintel:research::")
                                                  ":starintel:research:"
                                                  tags)
                                              status)))
      (with-open-file (out path :direction :output :external-format :utf-8)
        (write-string text out))
      path)))

(defun append-research-update (path &key findings sources repositories commits
                                            design-files next-action)
  "Append one bounded research update block to PATH following the canonical
update shape (Research update / Sources / Repositories Reviewed / Commits
Reviewed / Affected Design Files / Next Action)."
  (let* ((now (local-time:now))
         (timestamp (local-time:format-rfc3339-timestring nil now))
         (date (subseq timestamp 0 10))
         (lines (list "" (format nil "** DONE Research update ~a" timestamp))))
    (push (format nil "- ~a" (or findings "TODO")) lines)
    (push " " lines)
    (push "*** Sources" lines)
    (dolist (source (or sources '("TODO")))
      (push (format nil "- Retrieved ~a: ~a" date source) lines))
    (push " " lines)
    (push "*** Repositories Reviewed" lines)
    (dolist (repo (or repositories '("None")))
      (push (format nil "- ~a" repo) lines))
    (push " " lines)
    (push "*** Commits Reviewed" lines)
    (dolist (commit (or commits '("None")))
      (push (format nil "- ~a" commit) lines))
    (push " " lines)
    (push "*** Affected Design Files" lines)
    (dolist (design (or design-files '("TODO")))
      (push (format nil "- ~a" design) lines))
    (push " " lines)
    (push "*** Next Action" lines)
    (push (or next-action "TODO") lines)
    (with-open-file (out path :direction :output :if-exists :append
                             :external-format :utf-8)
      (dolist (line (nreverse lines))
        (write-line line out)))
    path))

(defun read-research-node (path)
  "Return (values text keywords approval-state lifecycle)."
  (let ((text (uiop:read-file-string path)))
    (values text
            (parse-org-keywords text)
            (approval-state-of text)
            (lifecycle-status-of text))))

;;; ------------------------------------------------ ARDR issue transactions

(defun write-ardr-issue-transaction (store instance repository number slug
                                     title question)
  "Create the durable issue transaction node under
roam/research/ardr-issues/ARDR-ISSUE-<number>-<slug>.org. Every issue-backed
research request/pass MUST have exactly one transaction node; domain
research is linked, never duplicated."
  (let* ((dir (ensure-directories-exist
               (merge-pathnames "roam/research/ardr-issues/"
                                (org-roam-root store))))
         (name (format nil "ARDR-ISSUE-~a-~a.org" number slug))
         (path (merge-pathnames name dir)))
    (when (probe-file path)
      (return-from write-ardr-issue-transaction path))
    (let* ((relative (enough-namestring path (org-roam-root store)))
           (id (stable-file-id relative))
           (text (format nil ":PROPERTIES:
:ID:       ~a
:END:
#+title: ~a
#+description: ARDR issue transaction for ~a issue ~a#~a.
#+status: OPEN
#+filetags: :starintel:research:ardr-issue:~a:
#+approval_schema: prolog-rlm.research-approval.v1
#+approval_state: PENDING
#+approval_actor: research-approval-migration
#+approval_evidence: transaction node created by the Auto Research runtime
#+approval_base_commit: ~a
#+approval_base_blob: ~a
#+approval_decided_at: NONE
#+created: ~a
#+last_modified: ~a

* Request

~a

* Linked Research

- Linked domain research node for this request. Do not duplicate canonical research here.

* Approval Table

| Approval area | Required authority | State | Evidence required | Evidence reference |
|---------------+--------------------+-------+-------------------+--------------------|
| Research basis | Research reviewer | PENDING | Current primary-source verification | |
| Architecture | Project maintainer | NOT APPLICABLE | Issue transaction does not change architecture | Issue transaction |
| Security | Security reviewer | NOT APPLICABLE | Read-only research request | Issue transaction |
| Operations | Operator | NOT APPLICABLE | Runtime-managed request | Issue transaction |
| Implementation | Repository maintainer | NOT STARTED | Passing implementation, CI, and publication checks | |

* Changelog

| Date | Change | Author or actor | Evidence |
|------+--------+-----------------+----------|
| ~a | Issue transaction created by Auto Research runtime | ~a | Forgejo issue ~a#~a |

* Footnotes and Glossary

- *ARDR issue transaction*: The durable per-issue research transaction node linking a Forgejo request to research state.
"
                   id
                   (or title name)
                   instance repository number slug
                   (org-roam-base-commit store)
                   (org-roam-base-commit store)
                   (sar.i:now-iso8601) (sar.i:now-iso8601)
                   question
                   (subseq (sar.i:now-iso8601) 0 10)
                   "auto-research-runtime"
                   instance repository number)))
      (with-open-file (out path :direction :output :external-format :utf-8)
        (write-string text out))
      path)))

(defun find-ardr-issue-transaction (store instance repository number)
  "Locate the transaction node for the qualified issue identity; nil when
absent."
  (let* ((dir (merge-pathnames "roam/research/ardr-issues/"
                               (org-roam-root store)))
         (prefix (format nil "ARDR-ISSUE-~a-" number)))
    (when (probe-file dir)
      (block search
        (dolist (path (uiop:directory-files dir "*.org"))
          (declare (ignore path))
          nil)
        (dolist (path (uiop:directory-files dir))
          (when (and (equalp (pathname-type path) "org")
                     (search prefix (pathname-name path)))
            ;; confirm repository identity inside the file
            (let ((text (uiop:read-file-string path)))
              (when (and (search repository text)
                         (search instance text))
                (return-from search path)))))))))

;;; ------------------------------------------------------- lexical search

(defun org-lexical-search (store query &key project status tag (limit 12))
  "Port of scripts/search.py: lexical scoring over roam/**/*.org with
matched-term counts, title bonus (+12), description bonus (+6), capped at
10 per term. Returns scored results (path title description tags score)."
  (let* ((terms (mapcar (lambda (term)
                          (string-downcase term))
                        (remove-duplicates
                         (ppcre:all-matches-as-strings "[\\w.-]+" query)
                         :test #'string=)))
         (results))
    (when terms
      (let ((roam (merge-pathnames "roam/" (org-roam-root store))))
        (dolist (path (uiop:directory-files roam "*.org"))
          (declare (ignore path)))
        ;; rglob: walk recursively
        (labels ((walk (dir)
                   (dolist (file (uiop:directory-files dir "*.org"))
                     (process file))
                   (dolist (sub (uiop:subdirectories dir))
                     (walk sub)))
                 (process (path)
                   (let* ((text (ignore-errors (uiop:read-file-string path)))
                          (relative (and text (enough-namestring path (org-roam-root store)))))
                     (when (and text relative)
                       (when (and project
                                  (not (search project relative)))
                         (return-from process))
                       (let* ((keywords (parse-org-keywords text))
                              (filetags (gethash "filetags" keywords ""))
                              (tags (remove-if #'zerop (mapcar #'string-trim (ppcre:split ":" filetags))))
                              (title (or (gethash "title" keywords) (pathname-name path)))
                              (description (gethash "description" keywords ""))
                              (lower (string-downcase text))
                              (score 0)
                              (matched))
                         (when (and status
                                    (not (equalp (gethash "status" keywords "") status)))
                           (return-from process))
                         (when (and tag
                                    (not (every (lambda (wanted) (member wanted tags :test #'string=))
                                                (if (listp tag) tag (list tag)))))
                           (return-from process))
                         (dolist (term terms)
                           (let ((count (count term (ppcre:split "" lower) :test #'string=)))
                             (declare (ignore count)))
                           (let ((count (length (ppcre:all-matches term lower))))
                             (when (plusp count)
                               (push term matched)
                               (incf score (min count 10))
                               (when (search term (string-downcase title))
                                 (incf score 12))
                               (when (search term (string-downcase description))
                                 (incf score 6)))))
                         (when (plusp score)
                           (push (list :path relative
                                       :title title
                                       :description description
                                       :tags tags
                                       :score score
                                       :matched-terms (nreverse matched))
                                 results)))))))
          (walk roam))))
    (let ((sorted (sort results #'> :key (lambda (r) (getf r :score)))))
      (subseq sorted 0 (min (max limit 1) (length sorted))))))

;;; ------------------------------------------------------ artifact protocol

(defmethod research-core:write-artifact ((store org-roam-store) path content &key)
  "Canonical Org writes are owned by the semantic operations and the
promotion gate; this method exists for the generic artifact protocol and
refuses paths outside the configured corpus."
  (let ((full (merge-pathnames path (org-roam-root store))))
    (unless (uiop:subpathp full (org-roam-root store))
      (error 'research-core:validation-error :message "path escapes corpus"))
    (ensure-directories-exist full)
    (with-open-file (out full :direction :output :if-exists :supersede
                              :external-format :utf-8)
      (write-string content out))
    full))

(defmethod research-core:read-artifact ((store org-roam-store) path &key)
  (uiop:read-file-string (merge-pathnames path (org-roam-root store))))

(defmethod research-core:search-artifacts ((store org-roam-store) query &key project limit)
  (org-lexical-search store query :project project :limit (or limit 12)))
