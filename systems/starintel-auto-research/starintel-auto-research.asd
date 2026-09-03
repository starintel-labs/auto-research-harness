;;;; starintel-auto-research.asd --- Auto Research adapters and runtime.
;;;;
;;;; Auto Research-specific code: Org-roam layout, canonical research
;;;; serialization, current approval metadata, ARDR issue transactions,
;;;; the project/init DSL, the semantic tool set, and the Forgejo request
;;;; plane. Generic machinery lives in cl-prolog-rlm, starintel-aradr,
;;;; and starintel-research-core.

(asdf:defsystem #:starintel-auto-research
  :description "Auto Research CL runtime on the reusable core"
  :author "StarIntel"
  :license "AGPL-3.0-only"
  :serial t
  :depends-on (#:alexandria
               #:cl-ppcre
               #:str
               #:babel
               #:com.inuoe.jzon
               #:dexador
               #:local-time
               #:cl-prolog-rlm
               #:starintel-aradr
               #:starintel-research-core)
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "org")
     (:file "approval")
     (:file "org-roam-store")
     (:file "ledgers")
     (:file "dsl")
     (:file "forgejo")
     (:file "tools")
     (:file "runtime")))))
