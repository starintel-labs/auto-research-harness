;;;; package.lisp --- packages for starintel-research-core.

(defpackage #:research-core/internal
  (:use #:cl)
  (:nicknames #:rc.i)
  (:export #:now-iso8601
           #:sha256-hex
           #:short-id
           #:dict))

(defpackage #:research-core/json
  (:use #:cl)
  (:nicknames #:rc.j))

(defpackage #:research-core
  (:use #:cl #:alexandria)
  (:nicknames #:rc)
  (:export
   ;; domain objects
   #:project
   #:make-project
   #:project-id
   #:project-display-name
   #:project-repository
   #:project-research-repository
   #:project-roam-slug
   #:project-method
   #:project-budget
   #:project-rlm-profile
   #:project-rlm-mode
   #:project-incoming-rules

   #:research-repository
   #:repository-id
   #:repository-path
   #:repository-artifact-store
   #:repository-event-store
   #:repository-forgejo

   #:research-request
   #:request-id
   #:request-project
   #:request-question
   #:request-source
   #:request-issue-ref
   #:request-status
   #:request-run-ids
   #:request-created-at

   #:research-run
   #:run-id
   #:run-request-id
   #:run-project
   #:run-status
   #:run-budget
   #:run-created-at

   #:budget
   #:make-budget
   #:budget-wall-clock-seconds
   #:budget-input-tokens
   #:budget-output-tokens
   #:budget-max-candidates
   #:budget-max-active-branches
   #:budget-max-depth

   #:principal
   #:make-principal
   #:principal-id
   #:principal-allowed-p

   #:issue-ref
   #:make-issue-ref
   #:issue-ref-kind
   #:issue-ref-instance
   #:issue-ref-repository
   #:issue-ref-number
   #:issue-ref-idempotency-key

   ;; capabilities
   #:capability-name
   #:known-capabilities
   #:check-capability

   ;; conditions
   #:core-error
   #:authorization-error
   #:authentication-error
   #:validation-error
   #:transition-error

   ;; event store
   #:event-store
   #:append-event
   #:replay-events
   #:make-jsonl-event-store
   #:make-event
   #:event-id
   #:event-type
   #:event-entity-id
   #:event-previous-state
   #:event-next-state
   #:event-timestamp
   #:event-actor
   #:event-config-hash
   #:event-payload-hash
   #:event-evidence

   ;; artifact/issue/event/index adapters
   #:artifact-store
   #:write-artifact
   #:read-artifact
   #:search-artifacts
   #:issue-source
   #:runtime-state

   ;; protocols (generic operations)
   #:find-project
   #:submit-research
   #:get-request
   #:get-run
   #:list-runs
   #:cancel-run
   #:record-approval
   #:promote
   #:process-forgejo-event
   #:run-pending
   #:record-event
   #:replay-events

   ;; in-memory runtime
   #:runtime
   #:make-runtime
   #:runtime-projects
   #:runtime-store
   #:runtime-registry))
