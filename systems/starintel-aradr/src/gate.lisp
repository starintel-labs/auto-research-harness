;;;; gate.lisp --- deterministic checks, vetoes, and the promotion gate.
;;;;
;;;; Ordinary code performs ordinary checks; no LLM calls are spent on
;;;; them. A numeric score can never override a veto. Promotion requires
;;;; EVALUATED state, no unresolved vetoes, thresholds met, and rejected
;;;; alternatives recorded. Promotion never activates an implementation
;;;; slot and never implies implementation approval.

(in-package #:starintel-aradr)

;;; ------------------------------------------------------ deterministic veto

(define-condition deterministic-veto (error)
  ((rule :initarg :rule :reader veto-p)
   (detail :initarg :detail :initform nil :reader veto-detail))
  (:report (lambda (c s)
             (format s "deterministic veto [~a]: ~a"
                     (veto-p c) (veto-detail c)))))

(defun run-vetoes (candidate)
  "Evaluate every deterministic veto against CANDIDATE (an association
list). Returns the list of triggered veto rules; empty means no veto."
  (let ((vetoed)
        (value (lambda (key) (cdr (assoc key candidate)))))
    (labels ((veto (rule detail)
               (push (cons rule detail) vetoed)))
      ;; fabricated or unresolved citation: every evidence entry must be
      ;; resolvable (non-empty) and citations must exist for material claims
      (let ((citations (funcall value :evidence-for)))
        (unless (and (listp citations) citations)
          (veto :unresolved-citation "no recorded evidence"))
        (dolist (citation citations)
          (unless (and (stringp citation) (plusp (length citation)))
            (veto :unresolved-citation (format nil "empty citation: ~a" citation)))))
      ;; unsupported central claim: a hypothesis without predicted result
      ;; and falsifier cannot be supported
      (unless (and (funcall value :predicted-result)
                   (funcall value :falsifier))
        (veto :unsupported-central-claim
              "missing predicted-result or falsifier"))
      ;; hidden dependency: implementation sketch must declare acceptance
      (unless (funcall value :acceptance-criteria)
        (veto :hidden-dependency "no acceptance criteria declared"))
      ;; unbounded recursion or budget
      (when (and (assoc :estimated-cost candidate)
                 (null (funcall value :estimated-cost)))
        (veto :unbounded-budget "estimated cost missing"))
      ;; canonical write by a branch actor
      (when (eq (funcall value :writes-canonical) t)
        (veto :canonical-write-by-branch-actor
              "branch actors may not write canonical files"))
      ;; duplicate hypothesis without new evidence
      (when (and (funcall value :duplicate-of)
                 (null (funcall value :new-evidence)))
        (veto :duplicate-proposal "duplicate hypothesis without new evidence")))
    (nreverse vetoed)))

;;; --------------------------------------------------- deterministic checks

(defun check-required-fields (candidate required)
  "Every required key must be present and non-nil. CANDIDATE is an alist."
  (let ((missing))
    (dolist (field required)
      (unless (cdr (assoc field candidate))
        (push field missing)))
    (when missing
      (error 'deterministic-veto
             :rule :missing-required-fields
             :detail (format nil "missing: {~{~a~^ ~}}" missing)))
    t))

(defun check-parent-ids (candidate known-ids)
  "All parent ids must reference known candidates (zero, one, or two)."
  (let* ((parents (cdr (assoc :parent-ids candidate)))
         (parents (or parents nil)))
    (unless (<= 0 (length parents) 2)
      (error 'deterministic-veto :rule :invalid-parents
             :detail "candidates record zero, one, or two parents"))
    (dolist (parent parents)
      (unless (member parent known-ids :test #'string=)
        (error 'deterministic-veto :rule :invalid-parents
               :detail (format nil "unknown parent id: ~a" parent))))
    t))

(defun check-evidence (candidate)
  "Material claims require evidence; contrary evidence is explicit."
  (unless (cdr (assoc :evidence-for candidate))
    (error 'deterministic-veto :rule :missing-evidence :detail "no evidence"))
  (unless (assoc :evidence-against candidate)
    (error 'deterministic-veto :rule :missing-counterevidence
           :detail "evidence-against must be recorded explicitly"))
  t)

(defun check-counterevidence (candidate)
  "Every candidate must record contrary evidence and a falsifier."
  (unless (cdr (assoc :evidence-against candidate))
    (error 'deterministic-veto :rule :missing-counterevidence
           :detail "empty contrary evidence"))
  (unless (cdr (assoc :falsifier candidate))
    (error 'deterministic-veto :rule :missing-falsifier
           :detail "no falsifier recorded"))
  t)

(defun check-budget-scope (candidate budget)
  "Budget/scope compliance: depth within limits, one implementation slot
concept per run, bounded cost estimate."
  (let ((depth (cdr (assoc :parent-depth candidate))))
    (when (and depth budget (> depth (budget-max-depth budget)))
      (error 'deterministic-veto :rule :scope-violated
             :detail (format nil "parent depth ~a exceeds max ~a"
                             depth (budget-max-depth budget)))))
  t)

;;; ------------------------------------------------------------ promotion

(defun promotion-denied-reasons (candidate vetoes scores thresholds)
  "Collect every reason promotion must be denied:
- candidate must be EVALUATED;
- no unresolved deterministic veto;
- evidence and architecture scores above thresholds;
- rejected alternatives recorded;
- promoted candidates are immutable: one promotion per candidate."
  (let ((reasons))
    (unless (eq (cdr (assoc :state candidate)) :evaluated)
      (push (format nil "state ~a is not EVALUATED" (cdr (assoc :state candidate))) reasons))
    (dolist (veto vetoes)
      (push (format nil "veto: ~a" (car veto)) reasons))
    (let ((evidence-score (cdr (assoc :evidence scores)))
          (architecture-score (cdr (assoc :architecture scores))))
      (when (and evidence-score
                 (< evidence-score (getf thresholds :evidence-min 4)))
        (push (format nil "evidence score ~a below threshold" evidence-score) reasons))
      (when (and architecture-score
                 (< architecture-score (getf thresholds :architecture-min 4)))
        (push (format nil "architecture score ~a below threshold" architecture-score) reasons)))
    (unless (cdr (assoc :rejected-alternatives candidate))
      (push "rejected alternatives not recorded" reasons))
    (when (eq (cdr (assoc :already-promoted candidate)) t)
      (push "candidate already promoted (immutable)" reasons))
    (nreverse reasons)))

(defun promotion-eligible-p (candidate vetoes scores thresholds)
  (null (promotion-denied-reasons candidate vetoes scores thresholds)))

(define-condition promotion-denied (error)
  ((reasons :initarg :reasons :reader promotion-denied-reason-list))
  (:report (lambda (c s)
             (format s "promotion denied: ~{~a~^; ~}"
                     (promotion-denied-reason-list c)))))

(defun promotion-gate (candidate vetoes scores thresholds)
  "The only path to :promoted. Applies deterministic guards; denial raises
promotion-denied with the full reason list. It never activates an
implementation slot and never implies implementation approval."
  (let ((reasons (promotion-denied-reasons candidate vetoes scores thresholds)))
    (when reasons
      (error 'promotion-denied :reasons reasons))
    :promoted))
