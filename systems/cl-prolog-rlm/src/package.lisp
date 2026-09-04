;;;; package.lisp --- packages for cl-prolog-rlm.

(defpackage #:rlm/internal
  (:use #:cl)
  (:nicknames #:rlm.i)
  (:local-nicknames (:jz :com.inuoe.jzon))
  (:export
   ;; json helpers
   #:json-parse
   #:json-stringify
   #:jref
   #:jstr
   #:jget
   #:jmap
   #:jnum
   ;; util
   #:now-iso8601
   #:make-uuid
   #:keyword->string
   #:alist-keys))

(defpackage #:rlm
  (:use #:cl #:alexandria)
  (:export
   ;; tools
   #:rlm-tool
   #:tool-name
   #:tool-description
   #:tool-effect
   #:tool-side-effect
   #:tool-time-limit
   #:tool-max-output-bytes
   #:tool-parameters
   #:tool-handler
   #:make-tool
   #:define-rlm-tool
   ;; registry
   #:rlm-registry
   #:make-registry
   #:register-tool
   #:registry-tools
   #:find-tool
   ;; tool invocation
   #:tool-argument-error
   #:tool-output-error
   #:invoke-registry-tool
   #:tool-wire-schema
   ;; context
   #:rlm-context
   #:context-text
   #:context-bytes
   #:make-context
   ;; runs
   #:rlm-run
   #:run-id
   #:run-status
   #:run-result
   #:run-usage
   #:run-tool-calls
   #:run-events
   #:run-error
   #:run-started-at
   #:run-finished-at
   #:run-process
   #:run-registry
   #:run-transport
   #:run-context
   #:run-query
   ;; budget
   #:rlm-budget
   #:make-budget
   ;; provider
   #:make-provider
   ;; api
   #:run
   #:run-async
   #:cancel
   #:status
   #:default-registry
   #:default-transport
   #:*default-transport*
   #:make-subprocess-transport
   ;; conditions
   #:rlm-error
   #:rlm-transport-error
   #:rlm-protocol-error
   #:rlm-run-failed
   #:rlm-cancelled))
