;;;; package.lisp --- web package.

(defpackage #:starintel-auto-research-web
  (:use #:cl)
  (:local-nicknames (:bt :bordeaux-threads))
  (:local-nicknames (:jz :com.inuoe.jzon)
                    (:rc :research-core)
                    (:rci :research-core/internal)
                    (:ar :starintel-auto-research))
  (:export
   #:start-api
   #:stop-api
   #:*runtime*
   #:*auth-tokens*
   #:configure-principal
   #:clear-principals
   #:web-error))
