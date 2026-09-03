;;;; starintel-aradr.asd --- ARADR workflow semantics in Common Lisp.
;;;;
;;;; ARADR decides when semantic work is required and which transitions are
;;;; legal. The model never chooses arbitrary stage transitions, and the
;;;; candidate lifecycle fails closed on invalid transitions. Human gates
;;;; follow the canonical approval policy: lifecycle and approval are
;;;; different facts, and implementation is never auto-activated.

(asdf:defsystem #:starintel-aradr
  :description "ARADR runtime semantics: stages, candidate lifecycle, vetoes, promotion gate"
  :author "StarIntel"
  :license "AGPL-3.0-only"
  :serial t
  :depends-on (#:alexandria
               #:cl-ppcre)
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "stages")
     (:file "candidate")
     (:file "gate")))))
