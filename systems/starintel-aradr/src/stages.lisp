;;;; stages.lisp --- ARADR stage machine.
;;;;
;;;; REQUEST -> RESEARCH -> ADVERSARIAL REVIEW -> ANALYSIS/RESOLUTION
;;;;          -> DESIGN -> DESIGN REVIEW -> PROMOTION GATE
;;;;
;;;; Stage transitions are owned by CL: the model performs semantic work
;;;; only when CL schedules a stage that requires it.

(in-package #:starintel-aradr)

(defparameter +stages+
  '(:request :research :adversarial-review :analysis :design :design-review :promotion-gate)
  "High-level ARADR method stages, in canonical order.")

(defun all-stages () +stages+)

(defparameter +stage-transitions+
  '((:request . :research)
    (:research . :adversarial-review)
    (:adversarial-review . :analysis)
    (:analysis . :design)
    (:design . :design-review)
    (:design-review . :promotion-gate))
  "The only legal forward transitions. Every other transition is invalid.")

(defparameter +semantic-stages+ '(:research :adversarial-review :analysis)
  "Stages during which the ARADR runtime schedules semantic work (RLM).")

(defun stage-allowed-p (from to)
  "Is the transition FROM -> TO a legal ARADR stage transition?"
  (eq (cdr (assoc from +stage-transitions+)) to))

(defun next-stage (stage)
  "The stage following STAGE, or nil for the terminal stage."
  (cdr (assoc stage +stage-transitions+)))

(defun stage-requires-semantic-work-p (stage)
  (member stage +semantic-stages+))

(defun stage-completed-p (stage artifacts)
  "A stage is complete only when its required artifacts exist. ARTIFACTS is
an association list of stage -> artifact-present-p."
  (let ((required
          (case stage
            (:request '(:request-node))
            (:research '(:research-node))
            (:adversarial-review '(:adversarial-review-node))
            (:analysis '(:resolution-record))
            (:design '(:design-node))
            (:design-review '(:design-review-node))
            (:promotion-gate '(:promotion-record)))))
    (loop :for artifact :in required
          :always (rest (assoc artifact artifacts)))))
