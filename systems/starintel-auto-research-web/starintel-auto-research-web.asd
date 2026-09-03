;;;; starintel-auto-research-web.asd --- thin authenticated HTTP projection.
;;;;
;;;; The website HTTP API is a thin authenticated projection over the CL
;;;; application API. Handlers never format Org, never mutate ledgers,
;;;; never call Prolog-RLM, never implement ARADR transitions, and never
;;;; implement Forgejo business rules; they authenticate, check
;;;; capabilities, and call the runtime.

(asdf:defsystem #:starintel-auto-research-web
  :description "Website research API over the Auto Research runtime"
  :author "StarIntel"
  :license "AGPL-3.0-only"
  :serial t
  :depends-on (#:alexandria
               #:cl-ppcre
               #:com.inuoe.jzon
               #:babel
               #:hunchentoot
               #:bordeaux-threads
               #:starintel-research-core
               #:starintel-aradr
               #:starintel-auto-research)
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "auth")
     (:file "api")))))
