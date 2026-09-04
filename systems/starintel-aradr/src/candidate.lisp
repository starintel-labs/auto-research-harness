;;;; candidate.lisp --- the approved candidate state machine.
;;;;
;;;; QUEUED -> EXPLORING -> PROPOSING -> VALIDATING -> VIRTUAL-REVIEW
;;;;        -> EVALUATED -> PROMOTED | PRUNED | BLOCKED | UNRESOLVED
;;;;
;;;; Allowed exceptional transitions:
;;;;   - any active state -> BLOCKED when a declared dependency is unavailable
;;;;   - any active state -> PRUNED when budget is exhausted or scope violated
;;;;   - BLOCKED -> EXPLORING only after a recorded unblock event
;;;;   - EVALUATED -> PROPOSING only for one bounded aggregator revision
;;;;
;;;; Invalid transitions fail closed. A promoted candidate is immutable.

(in-package #:starintel-aradr)

(defparameter +candidate-states+
  '(:queued :exploring :proposing :validating :virtual-review :evaluated
    :promoted :pruned :blocked :unresolved)
  "Canonical candidate lifecycle states (STAR-RESEARCH-PIPELINE-001).")

(defun all-candidate-states () +candidate-states+)

(defparameter +normal-candidate-transitions+
  '((:queued . :exploring)
    (:exploring . :proposing)
    (:proposing . :validating)
    (:validating . :virtual-review)
    (:virtual-review . :evaluated)
    (:evaluated . :promoted)
    (:evaluated . :pruned)
    (:evaluated . :unresolved))
  "The canonical forward transitions.")

(defparameter +active-candidate-states+
  '(:queued :exploring :proposing :validating :virtual-review :evaluated :blocked))

(defun terminal-candidate-state-p (state)
  (member state '(:promoted :pruned :blocked :unresolved)))

(defun active-candidate-state-p (state)
  (member state +active-candidate-states+))

(defun candidate-transition-allowed-p (from to)
  "Fail-closed transition check: T only for canonical transitions and the
approved exceptional transitions."
  (or (candidate-transition-allowed-normal-p from to)
      (exceptional-transition-allowed-p from to :reason :unblock)
      (exceptional-transition-allowed-p from to :reason :revision)))

(defun candidate-transition-allowed-normal-p (from to)
  (member to (mapcar #'cdr
                     (remove-if-not (lambda (pair) (eq (car pair) from))
                                    +normal-candidate-transitions+))))

(defun exceptional-transition-allowed-p (from to &key reason)
  "Exceptional transitions per the approved design:
- any active state -> BLOCKED (dependency unavailable)
- any active state -> PRUNED   (budget exhausted or scope violated)
- BLOCKED -> EXPLORING only after a recorded unblock event
- EVALUATED -> PROPOSING only for one bounded aggregator revision"
  (cond
    ((and (member from +active-candidate-states+) (eq to :blocked))
     (eq reason :dependency-unavailable))
    ((and (member from +active-candidate-states+) (eq to :pruned))
     (member reason '(:budget-exhausted :scope-violated)))
    ((and (eq from :blocked) (eq to :exploring))
     (eq reason :unblock))
    ((and (eq from :evaluated) (eq to :proposing))
     (eq reason :revision))
    (t nil)))

(define-condition transition-error (error)
  ((from :initarg :from :reader transition-error-from)
   (to :initarg :to :reader transition-error-to))
  (:report (lambda (c s)
             (format s "invalid candidate transition: ~a -> ~a"
                     (transition-error-from c)
                     (transition-error-to c)))))

(defun candidate-transition (state to &key reason)
  "Enforce one transition; returns the new state. Fails closed on invalid
transitions."
  (cond
    ((candidate-transition-allowed-normal-p state to) to)
    ((exceptional-transition-allowed-p state to :reason reason) to)
    (t (error 'transition-error :from state :to to))))
