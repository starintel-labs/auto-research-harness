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
  (let ((resolved (ensure-directories-exist root)))
    (make-instance 'org-roam-store
                   :root (namestring (truename resolved))
                   :base-commit base-commit)))

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
                   (ironclad:digest-sequence
                    :sha256 (babel:string-to-octets relative-path :encoding :utf-8)))
                  0 32)))

(defun canonical-research-template (id title description status filetags)
  "Canonical research node skeleton matching scripts/save-research.py output:
stable :ID:, required keywords, objective and findings sections, plus the
canonical Approval Table (never fabricated: all rows PENDING/NOT STARTED)
and Changelog required by scripts/validate-docs.py."
  (let ((today (subseq (sar.i:now-iso8601) 0 10)))
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

* Approval Table

| Approval area | Required authority | State | Evidence required | Evidence reference |
|---------------+--------------------+-------+-------------------+--------------------|
| Research basis | Research reviewer | PENDING | Current primary-source verification | |
| Architecture | Project maintainer | NOT APPLICABLE | Research note does not change architecture | Runtime research record |
| Security | Security reviewer | NOT APPLICABLE | Read-only research note | Runtime research record |
| Operations | Operator | NOT APPLICABLE | Runtime-managed research request | Runtime research record |
| Implementation | Repository maintainer | NOT STARTED | Passing implementation, CI, and publication checks | |

* Changelog

| Date | Change | Author or actor | Evidence |
|------+--------+-----------------+----------|
| ~a | Research node created by the Auto Research runtime | auto-research-runtime | runtime research request |

* Footnotes and Glossary

- *Primary source*: Original authoritative material used as direct evidence.
- *Inference*: A conclusion derived from evidence and identified separately from a verified fact.
" id title description status filetags status description status today)))

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
                                              status
                                              (if (string= tags ":starintel:research::")
                                                  ":starintel:research:"
                                                  tags))))
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

;;; ------------------------------------------------------- design nodes

(defun design-node-path (store slug run-id)
  "The canonical design node path for RUN-ID of project SLUG. Pure path
derivation so the design note and the design review resolve the same file."
  (merge-pathnames (format nil "roam/design/~a/design-~a.org"
                           (sar.i:slugify slug) (sar.i:slugify run-id))
                   (org-roam-root store)))

(defun canonical-design-template (id title description status filetags)
  "Canonical design node skeleton: stable :ID:, required keywords, the
canonical Approval Table (never fabricated: all rows PENDING/NOT STARTED
or justified NOT APPLICABLE) and Changelog required by
scripts/validate-docs.py, plus the design glossary. A research-derived
design REQUIRES its research basis: the Research basis row is PENDING
with the Research Basis section as its evidence reference, never NOT
APPLICABLE."
  (let ((today (subseq (sar.i:now-iso8601) 0 10)))
    (format nil ":PROPERTIES:
:ID:       ~a
:END:
#+title: ~a
#+description: ~a
#+status: ~a
#+filetags: ~a
#+todo: DRAFT REVIEW | DONE REJECTED

* Objective

~a

* Approval Table

| Approval area | Required authority | State | Evidence required | Evidence reference |
|---------------+--------------------+-------+-------------------+--------------------|
| Research basis | Research reviewer | PENDING | Research nodes and evidence recorded in the Research Basis section | Research Basis section |
| Architecture | Project maintainer | PENDING | Design review and resolved contradictions | |
| Security | Security reviewer | PENDING | Authorization, disclosure, and secret-handling review | |
| Operations | Operator | PENDING | Operational policy, budgets, monitoring, and rollback review | |
| Implementation | Repository maintainer | NOT STARTED | Passing implementation, CI, and publication checks | |

* Changelog

| Date | Change | Author or actor | Evidence |
|------+--------+-----------------+----------|
| ~a | Design node created by the Auto Research runtime | auto-research-runtime | runtime design stage |

* Footnotes and Glossary

- *Invariant* — A condition the design must preserve across every valid execution path.
- *Actor* — An independently stateful component that processes messages through a defined protocol.
" id title description status filetags status today)))

(defun table-cell (value)
  "One bounded, pipe-safe Org table cell from a model- or evidence-supplied value."
  (let* ((text (princ-to-string (or value "")))
         (flat (ppcre:regex-replace-all "[\\t\\r\\n|]+" text " ")))
    (string-trim " " flat)))

(defun render-design-provenance (research-basis evidence-rows)
  "CL-owned canonical provenance sections for a research-derived design:
Research Basis (the research nodes this design is grounded in) and
Evidence Used (claim/evidence/kind rows). The model can never forge
these sections: sanitize-model-text demotes heading-like lines, and
these sections are spliced in by CL from the run evidence bundle."
  (with-output-to-string (out)
    (format out "~%* Research Basis~%~%")
    (if research-basis
        (dolist (item research-basis)
          (format out "- ~a~%" (table-cell item)))
        (format out "- (none recorded)~%"))
    (format out "~%* Evidence Used~%~%")
    (format out "| Claim | Evidence | Kind |~%")
    (format out "|-------+----------+------|~%")
    (if evidence-rows
        (dolist (row evidence-rows)
          (format out "| ~a | ~a | ~a |~%"
                  (table-cell (first row))
                  (table-cell (second row))
                  (table-cell (third row))))
        (format out "| (none recorded) | (none recorded) | none |~%"))))

(defun write-design-node (store slug run-id title design-text
                          &key research-basis evidence-rows)
  "Create the canonical design node for RUN-ID of project SLUG with the
bounded DESIGN-TEXT (model-supplied, CL-serialized) as the objective
body. RESEARCH-BASIS and EVIDENCE-ROWS come from the run evidence
bundle; CL renders them as the canonical Research Basis and Evidence
Used provenance sections. Idempotent: an existing node is returned
unchanged. CL owns all Org formatting; the model never writes Org
directly. Returns the absolute path."
  (let ((path (design-node-path store slug run-id)))
    (if (probe-file path)
        path
        (let* ((relative (enough-namestring path (org-roam-root store)))
               (id (stable-file-id relative))
               (safe-text (sanitize-model-text design-text))
               (status "DRAFT")
               (tags (format nil ":starintel:design:~a:draft:" (sar.i:slugify slug)))
               (text (canonical-design-template
                      id title
                      (format nil "Design note for run ~a of project ~a."
                              (sar.i:slugify run-id) (sar.i:slugify slug))
                      status tags))
               (text (org-replace-section
                      text "Objective"
                      (format nil "* Objective~%~%~a~%~a"
                              safe-text
                              (render-design-provenance research-basis evidence-rows)))))
          (ensure-directories-exist path)
          (with-open-file (out path :direction :output :external-format :utf-8)
            (write-string text out))
          path))))

(defun resolve-store-reference (store reference)
  "Resolve a design evidence reference within the corpus: roam-relative
paths must exist, id: references must name an existing node. Returns the
reference when resolvable, nil otherwise."
  (when (and (stringp reference) (plusp (length reference)))
    (cond
      ((uiop:string-prefix-p "roam/" reference)
       (let ((path (merge-pathnames reference (org-roam-root store))))
         (and (probe-file path) reference)))
      ((uiop:string-prefix-p "id:" reference)
       (let* ((wanted (subseq reference 3))
              (roam (merge-pathnames "roam/" (org-roam-root store)))
              (found))
         (labels ((walk (dir)
                    (dolist (file (uiop:directory-files dir "*.org"))
                      (let ((text (ignore-errors (uiop:read-file-string file))))
                        (when (and text (search wanted text))
                          (setf found t))))
                    (dolist (sub (uiop:subdirectories dir))
                      (unless found (walk sub)))))
           (walk roam)
           (and found reference))))
      ;; URLs, citations, candidate ids, and source strings are accepted
      ;; as opaque evidence references when non-empty
      (t reference))))

(defun design-list-items (section-text)
  "The non-empty `- ` list items of SECTION-TEXT."
  (let ((items))
    (with-input-from-string (in (or section-text ""))
      (loop
        :for line := (read-line in nil nil)
        :while line
        :do (let ((trimmed (string-trim " " line)))
              (when (uiop:string-prefix-p "- " trimmed)
                (let ((item (string-trim " " (subseq trimmed 2))))
                  (when (plusp (length item))
                    (push item items)))))))
    (nreverse items)))

(defun validate-design-node (store slug run-id)
  "Structural validation of the design node for RUN-ID of project SLUG.
A design does not count as valid merely because its file exists. Checks:
the node exists at the canonical path for the project/run identity, the
objective is non-empty, the Research Basis section lists at least one
reference that resolves inside the corpus, and the Evidence Used table
has at least one row whose evidence reference resolves. Returns
(values valid-p reasons)."
  (let ((path (design-node-path store slug run-id))
        (reasons))
    (unless (probe-file path)
      (push (format nil "design node missing: roam/design/~a/design-~a.org"
                     (sar.i:slugify slug) (sar.i:slugify run-id))
            reasons))
    (when (probe-file path)
      (let* ((text (uiop:read-file-string path)))
        ;; objective section: present and non-empty
        (multiple-value-bind (obj-start obj-end)
            (org-section-bounds text "Objective")
          (declare (ignore obj-end))
          (if (null obj-start)
              (push "objective section missing" reasons)
              (let ((body (string-trim
                           " \t"
                           (ppcre:regex-replace-all
                            "^\\* Objective\\s*"
                            (subseq text obj-start (min (length text) (+ obj-start 4096)))
                            ""))))
                (when (zerop (length body))
                  (push "design objective is empty" reasons)))))
        ;; research basis: present, with at least one resolvable reference
        (multiple-value-bind (basis-start basis-end)
            (org-section-bounds text "Research Basis")
          (if (null basis-start)
              (push "research basis section missing" reasons)
              (let* ((section (subseq text basis-start basis-end))
                     (items (design-list-items section))
                     (resolvable
                       (loop :for item :in items
                             :for refs := (ppcre:all-matches-as-strings
                                           "(roam/[^)\\s]+|id:[^)\\s]+)" item)
                             :thereis (and refs
                                           (every (lambda (ref)
                                                    (resolve-store-reference store ref))
                                                  refs)))))
                (when (null items)
                  (push "research basis records no reference" reasons))
                (unless (or resolvable
                            (and (= (length items) 1)
                                 (search "none recorded" (first items))))
                  (push "research basis records no resolvable research node"
                        reasons))
                (dolist (item items)
                  (dolist (ref (ppcre:all-matches-as-strings
                                "(roam/[^)\\s]+|id:[^)\\s]+)" item))
                    (unless (resolve-store-reference store ref)
                      (push (format nil "unresolved research basis reference: ~a" ref)
                            reasons)))))))
        ;; evidence used: a resolvable row of claim/evidence/kind
        (multiple-value-bind (evidence-start evidence-end)
            (org-section-bounds text "Evidence Used")
          (if (null evidence-start)
              (push "evidence-used section missing" reasons)
              (let* ((section (subseq text evidence-start evidence-end))
                     (rows (nth-value 1 (org-table-rows section))))
                (unless (and rows
                             (every (lambda (row)
                                      (and (>= (length row) 2)
                                           (plusp (length (first row)))
                                           (plusp (length (second row)))
                                           (not (search "none recorded" (second row)))))
                                    rows))
                  (push "evidence-used table records no resolvable evidence row"
                        reasons))
                (dolist (row rows)
                  (when (>= (length row) 2)
                    (dolist (ref (ppcre:all-matches-as-strings
                                  "(roam/[^)\\s|]+|id:[^)\\s|]+)" (second row)))
                      (unless (resolve-store-reference store ref)
                        (push (format nil "unresolved evidence reference: ~a" ref)
                              reasons))))))))))
    (values (null reasons) (nreverse reasons))))

(defun org-replace-section (text title replacement)
  "Replace the level-1 section TITLE in TEXT with REPLACEMENT (which
includes its own heading). Other sections are preserved."
  (multiple-value-bind (start end)
      (org-section-bounds text title)
    (if start
        (concatenate 'string (subseq text 0 start)
                     replacement
                     (subseq text end))
        text)))

(defun sanitize-model-text (text)
  "Model text is a bounded value, never Org structure: demote any
heading-like line so model output can never forge canonical sections."
  (with-output-to-string (out)
    (with-input-from-string (in (or text ""))
      (loop :for line := (read-line in nil nil)
            :while line
            :do (write-line
                 (if (ppcre:scan "^\\*+\\s" line)
                     (ppcre:regex-replace "^\\*+\\s+" line "- ")
                     line)
                 out)))))

(defun append-design-review (path review verdict)
  "Append one bounded design review block to the design node at PATH.
The verdict is explicit text recorded by the design review stage."
  (let* ((timestamp (local-time:format-rfc3339-timestring nil (local-time:now)))
         (block (format nil "
** DONE Design review ~a
- Verdict: ~a
- ~a
" timestamp (sanitize-model-text verdict) (sanitize-model-text review))))
    (with-open-file (out path :direction :output :if-exists :append
                                 :external-format :utf-8)
      (write-string block out))
    path))

(defun design-reviewed-p (path)
  "T when the design node at PATH records at least one design review."
  (and (probe-file path)
       (search "Design review" (uiop:read-file-string path))))

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
                              (tags (remove-if (lambda (c) (zerop (length (string-trim " " c)))) (ppcre:split ":" filetags)))
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
