;;;; aradr-tests.lisp --- stage machine, candidate lifecycle, vetoes, gate.

(in-package #:starintel-auto-research-tests)

(in-suite auto-research-suite)

(test aradr-stage-transitions
  (is-true (aradr:stage-allowed-p :request :research))
  (is-true (aradr:stage-allowed-p :research :adversarial-review))
  (is-true (aradr:stage-allowed-p :design-review :promotion-gate))
  ;; arbitrary model-chosen transitions are illegal
  (is-false (aradr:stage-allowed-p :request :design))
  (is-false (aradr:stage-allowed-p :promotion-gate :research))
  (is-true (aradr:stage-requires-semantic-work-p :research))
  (is-false (aradr:stage-requires-semantic-work-p :promotion-gate)))

(test candidate-allowed-transitions
  ;; canonical forward path
  (loop :for (from to) :in '((:queued :exploring)
                             (:exploring :proposing)
                             (:proposing :validating)
                             (:validating :virtual-review)
                             (:virtual-review :evaluated))
        :do (is-true (aradr:candidate-transition-allowed-p from to)))
  (is-true (aradr:candidate-transition-allowed-p :evaluated :promoted))
  (is-true (aradr:candidate-transition-allowed-p :evaluated :pruned))
  (is-true (aradr:candidate-transition-allowed-p :evaluated :unresolved)))

(test candidate-invalid-transitions-rejected
  (signals aradr:transition-error (aradr:candidate-transition :queued :evaluated))
  (signals aradr:transition-error (aradr:candidate-transition :queued :promoted))
  (signals aradr:transition-error (aradr:candidate-transition :exploring :virtual-review))
  (signals aradr:transition-error (aradr:candidate-transition :promoted :evaluated))
  (signals aradr:transition-error
    (aradr:candidate-transition :blocked :exploring))) ; needs :unblock reason

(test candidate-exceptional-transitions
  (is (eq :blocked (aradr:candidate-transition :proposing :blocked
                                               :reason :dependency-unavailable)))
  (is (eq :exploring (aradr:candidate-transition :blocked :exploring
                                                 :reason :unblock)))
  (is (eq :pruned (aradr:candidate-transition :validating :pruned
                                              :reason :budget-exhausted)))
  (is (eq :proposing (aradr:candidate-transition :evaluated :proposing
                                                 :reason :revision))))

(test deterministic-vetoes-block-promotion
  (let ((candidate (list (cons :state :evaluated)
                         (cons :evidence-for nil)      ; no evidence -> veto
                         (cons :predicted-result "x")
                         (cons :falsifier "y")
                         (cons :acceptance-criteria '("test"))
                         (cons :estimated-cost 100)
                         (cons :rejected-alternatives '("alt-a")))))
    (is-true (aradr:run-vetoes candidate))
    ;; promotion gate must deny
    (signals aradr:promotion-denied
      (aradr:promotion-gate candidate
                            (aradr:run-vetoes candidate)
                            (list (cons :evidence 5) (cons :architecture 5))
                            (list :evidence-min 4 :architecture-min 4)))))

(test clean-candidate-with-thresholds-may-promote
  (let ((candidate (list (cons :state :evaluated)
                         (cons :evidence-for '("verified source"))
                         (cons :evidence-against '("none recorded"))
                         (cons :predicted-result "observable")
                         (cons :falsifier "rejecting observation")
                         (cons :acceptance-criteria '("criterion"))
                         (cons :estimated-cost 100)
                         (cons :rejected-alternatives '("alt-a")))))
    (is (null (aradr:run-vetoes candidate)))
    (is (eq :promoted
            (aradr:promotion-gate candidate
                                  nil
                                  (list (cons :evidence 5) (cons :architecture 4))
                                  (list :evidence-min 4 :architecture-min 4))))))

(test scores-cannot-override-veto
  ;; perfect scores must NOT rescue a vetoed candidate
  (let ((candidate (list (cons :state :evaluated)
                         (cons :evidence-for nil)   ; veto: no evidence
                         (cons :predicted-result "x")
                         (cons :falsifier "y")
                         (cons :acceptance-criteria '("t"))
                         (cons :estimated-cost 1)
                         (cons :rejected-alternatives '("alt")))))
    (is-true (aradr:run-vetoes candidate))
    (signals aradr:promotion-denied
      (aradr:promotion-gate candidate
                            (aradr:run-vetoes candidate)
                            (list (cons :evidence 5) (cons :architecture 5))
                            (list :evidence-min 1 :architecture-min 1)))))

(test unevaluated-candidate-cannot-promote
  (let ((candidate (list (cons :state :virtual-review)
                         (cons :evidence-for '("src"))
                         (cons :evidence-against '("c"))
                         (cons :predicted-result "p")
                         (cons :falsifier "f")
                         (cons :acceptance-criteria '("a"))
                         (cons :estimated-cost 1)
                         (cons :rejected-alternatives '("x")))))
    (signals aradr:promotion-denied
      (aradr:promotion-gate candidate nil nil (list :evidence-min 4 :architecture-min 4)))))

(test promotion-does-not-imply-implementation
  ;; Promotion never auto-activates an implementation slot: the gate
  ;; returns a state, never touches roam/implement/, and the runtime has
  ;; no implementation-activation API.
  (is-false (find-symbol "ACTIVATE-IMPLEMENTATION" :starintel-auto-research))
  (is-false (find-symbol "ACTIVATE-IMPLEMENTATION" :starintel-aradr)))
