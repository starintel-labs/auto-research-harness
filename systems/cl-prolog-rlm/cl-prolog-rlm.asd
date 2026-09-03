;;;; cl-prolog-rlm.asd --- generic Common Lisp harness for Prolog-RLM.
;;;;
;;;; This system knows nothing about Auto Research, ARADR, Org, Forgejo, or
;;;; the website. It provides run/cancel/status, a bounded context builder,
;;;; a tool definition abstraction, and a hidden subprocess transport that
;;;; speaks a JSONL session protocol to the Prolog-RLM runtime.

(asdf:defsystem #:cl-prolog-rlm
  :description "Generic Common Lisp harness for Prolog-RLM (:direct mode)"
  :author "StarIntel"
  :license "AGPL-3.0-only"
  :serial t
  :depends-on (#:alexandria
               #:com.inuoe.jzon
               #:babel
               #:bordeaux-threads
               #:local-time
               #:ironclad)
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "json")
     (:file "tool")
     (:file "transport")
     (:file "run")))))
