;;;; package.lisp --- ARADR package.

(defpackage #:starintel-aradr
  (:use #:cl #:alexandria)
  (:nicknames #:aradr)
  (:export
   ;; stages
   #:adr-stage
   #:all-stages
   #:stage-allowed-p
   #:next-stage
   #:stage-requires-semantic-work-p
   #:stage-completed-p

   ;; candidate lifecycle
   #:candidate-state
   #:all-candidate-states
   #:candidate-transition
   #:candidate-transition-allowed-p
   #:terminal-candidate-state-p
   #:active-candidate-state-p
   #:exceptional-transition-allowed-p

   ;; vetoes
   #:deterministic-veto
   #:veto-p
   #:veto-detail
   #:run-vetoes
   #:transition-error
   #:transition-error-from
   #:transition-error-to
   ;; deterministic checks
   #:check-required-fields
   #:check-parent-ids
   #:check-evidence
   #:check-counterevidence
   #:check-budget-scope

   ;; promotion gate
   #:promotion-eligible-p
   #:promotion-gate
   #:promotion-denied
   #:promotion-denied-reasons
   #:promotion-denied-reason-list))
