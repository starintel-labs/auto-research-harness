;;;; prompts.lisp --- project prompt profiles and stage context files.
;;;;
;;;; A project may declare a prompt profile: prompts/<profile>/ holds
;;;; context.md (loaded for EVERY stage) plus one optional file per stage
;;;; (research.md, adversarial-review.md, analysis.md, design.md,
;;;; design-review.md). Precedence is explicit: the general profile
;;;; context comes first, the stage-specific file is appended after it;
;;;; missing files are skipped; no file is required. Profile content is
;;;; bounded and is spliced into the per-stage model context by the
;;;; runtime; the model never reads prompt files directly.

(in-package #:starintel-auto-research)

(defparameter +prompt-stage-files+
  '((:request . "request.md")
    (:research . "research.md")
    (:adversarial-review . "adversarial-review.md")
    (:analysis . "analysis.md")
    (:design . "design.md")
    (:design-review . "design-review.md")
    (:promotion-gate . "promotion-gate.md"))
  "Stage -> optional profile file name.")

(defun prompt-profile-dir (runtime project)
  "The prompts/<profile>/ directory beneath the corpus root."
  (merge-pathnames
   (format nil "prompts/~a/" (sar.i:slugify (project-config-prompt-profile project)))
   (pathname (ar-root runtime))))

(defun project-prompt-files (runtime project stage)
  "Existing prompt files for PROJECT and STAGE in precedence order:
profile context.md first, then the stage file. Missing files are skipped."
  (let ((dir (prompt-profile-dir runtime project)))
    (loop :for name :in (list "context.md"
                              (cdr (assoc stage +prompt-stage-files+)))
          :for path := (and name (merge-pathnames name dir))
          :when (and path (uiop:file-exists-p path))
            :collect path)))

(defun project-prompt-context (runtime project stage &key (max-bytes 2048))
  "Bounded concatenation of the project prompt profile files for STAGE.
General profile context precedes stage-specific content. Returns nil when
the profile contributes nothing."
  (let ((files (project-prompt-files runtime project stage)))
    (when files
      (let ((per-file (max 256 (floor max-bytes (length files)))))
        (with-output-to-string (out)
          (dolist (path files)
            (let ((text (ignore-errors (uiop:read-file-string path))))
              (when (and text (plusp (length (string-trim " " text))))
                (format out "<project-prompt profile=~s file=~s>~%~a~%</project-prompt>~%"
                        (project-config-prompt-profile project)
                        (file-namestring path)
                        (excerpt text per-file))))))))))
