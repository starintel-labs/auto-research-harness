;;;; starintel-research-core.asd --- reusable research runtime core.
;;;;
;;;; First-class domain objects, the append-only event/replay layer, the
;;;; principal/capability model, and the core generic operations that CLI,
;;;; HTTP, cron, and agents must call instead of reimplementing workflow.
;;;; Storage adapters are injectable so StarIntel-native stores can replace
;;;; the initial Org/JSONL adapters without changing this layer.

(asdf:defsystem #:starintel-research-core
  :description "Auto Research reusable core: domain, events, capabilities, protocols"
  :author "StarIntel"
  :license "AGPL-3.0-only"
  :serial t
  :depends-on (#:alexandria
               #:cl-ppcre
               #:str
               #:babel
               #:bordeaux-threads
               #:com.inuoe.jzon
               #:ironclad
               #:local-time)
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "json")
     (:file "util")
     (:file "domain")
     (:file "principal")
     (:file "events")
     (:file "protocols")))))
