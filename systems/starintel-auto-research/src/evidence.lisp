;;;; evidence.lisp --- the canonical per-run evidence bundle.
;;;;
;;;; One semantic representation of accumulated run evidence. Stages do
;;;; not reconstruct state from prose: semantic tools record structured
;;;; evidence entries against the active run, and downstream stages
;;;; (DESIGN, ANALYSIS, DESIGN REVIEW) receive a deterministic bounded
;;;; projection of this bundle. NO EVIDENCE -> NO DESIGN is enforced
;;;; against the same bundle, so the gate and the context cannot diverge.

(in-package #:starintel-auto-research)

(defparameter *ar-current-run-id* nil
  "Run-id attributed to semantic tool activity. Bound by the stage
scheduler around semantic stage execution; tools record their evidence
against this run. Unbound (nil) outside a run: tools still work but do
not attribute evidence.")

(defparameter +evidence-entry-limit+ 24
  "Maximum evidence entries retained per run and per kind.")

(defparameter +evidence-text-limit+ 400
  "Maximum characters of one evidence entry's text in the bundle.")

(defun ar-resolve-artifact-path (runtime path)
  "Resolve PATH (absolute or store-relative) against the corpus root;
nil when it does not exist."
  (probe-file (merge-pathnames path (org-roam-root (ar-store runtime)))))

(defun ar-note-research-artifact (runtime run-id path)
  "Record PATH as a research artifact touched by RUN-ID (deduplicated,
bounded). The stored form is the roam-relative namestring."
  (when (and runtime run-id path)
    (let* ((root (org-roam-root (ar-store runtime)))
           (full (ar-resolve-artifact-path runtime path)))
      (when full
        (let ((relative (enough-namestring full root))
              (table (ar-run-artifacts-slot runtime)))
          (unless (member relative (gethash run-id table) :test #'string=)
            (push relative (gethash run-id table))
            ;; oldest entries are dropped first
            (let ((entries (gethash run-id table)))
              (when (> (length entries) +evidence-entry-limit+)
                (setf (gethash run-id table)
                      (subseq entries 0 +evidence-entry-limit+))))))))))

(defun ar-note-evidence (runtime run-id kind path text)
  "Record one structured evidence entry (KIND, PATH, TEXT) for RUN-ID.
KIND is one of :finding :source :counterevidence :unresolved-question
:repository-observation. Entries are bounded; the newest are kept."
  (when (and runtime run-id kind)
    (let ((entry (list :kind kind
                       :path (and path
                                  (ar-resolve-artifact-path runtime path)
                                  (ignore-errors
                                   (enough-namestring
                                    (ar-resolve-artifact-path runtime path)
                                    (org-roam-root (ar-store runtime)))))
                       :text (and text (excerpt (princ-to-string text)
                                                +evidence-text-limit+)))))
      (push entry (gethash run-id (ar-run-evidence-slot runtime)))
      (let ((entries (gethash run-id (ar-run-evidence-slot runtime))))
        (when (> (length entries) +evidence-entry-limit+)
          (setf (gethash run-id (ar-run-evidence-slot runtime))
                (subseq entries 0 +evidence-entry-limit+)))))))

(defun evidence-of-kind (entries kind)
  (loop :for entry :in entries
        :when (eq (getf entry :kind) kind)
          :collect entry))

(defun run-evidence-bundle (runtime run)
  "The canonical evidence bundle for RUN: identity, research artifacts,
findings, sources, counterevidence, unresolved questions, repository
observations, and the run's candidates with their evidence fields."
  (let* ((run-id (research-core:run-id run))
         (request (ar-get-request runtime (research-core:run-request-id run)))
         (project (and request
                       (find-project-config
                        (project-config-id (research-core:request-project request)))))
         (entries (gethash run-id (ar-run-evidence-slot runtime)))
         (candidates (ar-run-candidates runtime run-id)))
    (list :run-id run-id
          :project-id (and project (project-config-id project))
          :question (and request (research-core:request-question request))
          :research-artifacts
          (loop :for path :in (gethash run-id (ar-run-artifacts-slot runtime))
                :collect (list :path path
                               :title (or (ignore-errors
                                           (gethash "title"
                                                    (parse-org-keywords
                                                     (uiop:read-file-string
                                                      (merge-pathnames
                                                       path
                                                       (org-roam-root
                                                        (ar-store runtime)))))))
                                          "")))
          :findings (evidence-of-kind entries :finding)
          :sources (evidence-of-kind entries :source)
          :counterevidence (evidence-of-kind entries :counterevidence)
          :unresolved-questions (evidence-of-kind entries :unresolved-question)
          :repository-observations (evidence-of-kind entries :repository-observation)
          :candidates
          (loop :for candidate :in candidates
                :collect (list :id (cdr (assoc :id candidate))
                               :hypothesis (cdr (assoc :hypothesis candidate))
                               :evidence (cdr (assoc :evidence-for candidate))
                               :falsifier (cdr (assoc :falsifier candidate))
                               :acceptance-criteria (cdr (assoc :acceptance-criteria candidate))
                               :rejected-alternatives (cdr (assoc :rejected-alternatives candidate))
                               :reviews (length (cdr (assoc :reviews candidate))))))))

(defun evidence-grounding-problem (runtime run)
  "Nil when RUN carries the minimum research basis required before the
DESIGN stage; otherwise a deterministic reason string. This is the
NO EVIDENCE -> NO DESIGN gate: it is evaluated on the same bundle that
feeds the design context, so a design can never run on nothing."
  (let ((bundle (run-evidence-bundle runtime run)))
    (cond
      ((or (null (getf bundle :run-id))
           (null (getf bundle :project-id)))
       "run lacks a valid project/run identity")
      ((null (getf bundle :question))
       "run lacks a recorded question")
      ((null (getf bundle :research-artifacts))
       "no research artifact was recorded during the run")
      ((and (null (getf bundle :findings))
            (null (getf bundle :sources))
            (null (getf bundle :counterevidence)))
       "no finding, source, or counterevidence was recorded")
      ((and (null (getf bundle :candidates))
            (null (getf bundle :findings)))
       "no candidate or explicit research conclusion was recorded"))))

(defun render-evidence-entry (entry)
  (format nil "~a~@[ [~a]~]" (or (getf entry :text) "") (getf entry :path)))

(defun design-basis-items (runtime bundle)
  "Research Basis list items for a design node: one per research
artifact recorded during the run, in canonical `path (title)' form."
  (loop :for artifact :in (getf bundle :research-artifacts)
        :collect (format nil "~a (~a)" (getf artifact :path) (getf artifact :title))))

(defun design-evidence-rows (bundle)
  "Evidence Used rows (claim, evidence, kind) derived deterministically
from the run evidence bundle. Findings, sources, counterevidence, and
candidate claims each become a row citing their recorded evidence or
artifact path. Rows are bounded; nothing is invented."
  (let ((rows))
    (loop :for finding :in (getf bundle :findings)
          :repeat 6
          :do (push (list (or (getf finding :text) "")
                          (or (getf finding :path) "recorded finding")
                          "finding")
                    rows))
    (loop :for source :in (getf bundle :sources)
          :repeat 4
          :do (push (list (or (getf source :text) "")
                          (or (getf source :path) "recorded source")
                          "source")
                    rows))
    (loop :for contrary :in (getf bundle :counterevidence)
          :repeat 4
          :do (push (list (or (getf contrary :text) "")
                          (or (getf contrary :path) "recorded counterevidence")
                          "counterevidence")
                    rows))
    (loop :for candidate :in (getf bundle :candidates)
          :repeat 4
          :do (push (list (excerpt (or (getf candidate :hypothesis) "") 160)
                          (format nil "~a [~a]"
                                  (or (first (getf candidate :evidence))
                                      (or (getf candidate :falsifier) "recorded candidate"))
                                  (or (getf candidate :id) ""))
                          "candidate")
                    rows))
    (nreverse rows)))

(defun project-evidence-bundle (bundle &key (max-bytes 5120))
  "Deterministic bounded text projection of a run evidence bundle for a
model context. Semantic evidence is preserved (identity, candidates,
findings, sources, counterevidence, unresolved questions, repository
observations); the projection never contains raw transcripts and is
truncated at MAX-BYTES with an explicit marker."
  (let* ((candidates (getf bundle :candidates))
         (sections
           (list
            (format nil "run: ~a~%project: ~a~%question: ~a"
                    (getf bundle :run-id)
                    (getf bundle :project-id)
                    (getf bundle :question))
            (when candidates
              (format nil "candidates:~{~%~a~}"
                      (loop :for candidate :in candidates
                            :collect (format nil
                                             "- ~a: hypothesis: ~a; falsifier: ~a~@[; acceptance: ~a~]~@[; evidence: ~{~a~^; ~}~]~@[; rejected alternative: ~{~a~^; ~}~] (reviews: ~a)"
                                             (getf candidate :id)
                                             (getf candidate :hypothesis)
                                             (getf candidate :falsifier)
                                             (first (getf candidate :acceptance-criteria))
                                             (getf candidate :evidence)
                                             (getf candidate :rejected-alternatives)
                                             (getf candidate :reviews)))))
            (when (getf bundle :findings)
              (format nil "findings:~{~%~a~}"
                      (mapcar #'render-evidence-entry (getf bundle :findings))))
            (when (getf bundle :sources)
              (format nil "sources:~{~%~a~}"
                      (mapcar #'render-evidence-entry (getf bundle :sources))))
            (when (getf bundle :counterevidence)
              (format nil "counterevidence:~{~%~a~}"
                      (mapcar #'render-evidence-entry (getf bundle :counterevidence))))
            (when (getf bundle :unresolved-questions)
              (format nil "unresolved questions:~{~%~a~}"
                      (mapcar #'render-evidence-entry (getf bundle :unresolved-questions))))
            (when (getf bundle :repository-observations)
              (format nil "repository observations:~{~%~a~}"
                      (mapcar #'render-evidence-entry (getf bundle :repository-observations))))
            (when (getf bundle :research-artifacts)
              (format nil "research artifacts:~{~%~a~}"
                      (loop :for artifact :in (getf bundle :research-artifacts)
                            :collect (format nil "- ~a (~a)"
                                             (getf artifact :path)
                                             (getf artifact :title)))))))
         (text (with-output-to-string (out)
                 (dolist (section sections)
                   (when section
                     (write-string section out)
                     (terpri out))))))
    (if (> (length text) max-bytes)
        (concatenate 'string
                     (subseq text 0 max-bytes)
                     (format nil "~%[evidence projection truncated]"))
        text)))
