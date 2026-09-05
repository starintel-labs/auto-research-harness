;;;; package.lisp --- packages for starintel-auto-research.

(defpackage #:starintel-auto-research/internal
  (:use #:cl)
  (:nicknames #:sar.i)
  (:export #:now-iso8601
           #:slugify
           #:short-id
           #:dict))

(defpackage #:starintel-auto-research
  (:use #:cl #:alexandria)
  (:nicknames #:sar)
  (:export
   ;; org keyword layer
   #:parse-org-keywords
   #:upsert-org-keyword
   #:org-section-bounds
   #:org-table-rows
   ;; approval
   #:+approval-schema+
   #:+approval-fields+
   #:+approval-states+
   #:canonical-approval-p
   #:approval-metadata-from-keywords
   #:validate-approval-metadata
   #:render-approval-keywords
   #:approval-state-of
   #:lifecycle-status-of

   ;; org-roam artifact store
   #:org-roam-store
   #:make-org-roam-store
   #:ensure-roam-layout
   #:mirror-roam-structure
   #:roam-project-dir
   #:write-research-node
   #:write-design-node
   #:design-node-path
   #:append-design-review
   #:design-reviewed-p
   #:append-research-update
   #:read-research-node
    #:write-ardr-issue-transaction
    #:find-ardr-issue-transaction
    #:org-lexical-search
    #:validate-design-node
    #:render-design-provenance

   ;; run/candidate ledgers
   #:run-ledger
   #:candidate-ledger
   #:make-run-ledger
   #:make-candidate-ledger
   #:ledger-append
   #:ledger-replay
   #:ledger-record
   #:ledger-path

   ;; DSL
   #:define-research-repository
   #:define-budget
   #:define-project
   #:budget-profile
   #:research-repository-config
   #:research-repository-forgejo
   #:project-config
   #:project-configs
   #:budget-profiles
   #:research-repositories
   #:find-budget-profile
   #:find-research-repository
   #:find-project-config
   #:find-project-config-or-fail
   #:clear-configs
   #:unknown-project-error
   #:project-config-id
   #:project-config-repository
   #:project-config-source-path
   #:project-config-roam-slug
   #:project-config-rlm-mode
   #:project-config-method
   #:project-config-budget-name
   #:project-config-research-repository
   #:project-config-prompt-profile
   #:project-config-incoming

   ;; project prompt profiles
   #:prompt-profile-dir
   #:project-prompt-files
   #:project-prompt-context

   ;; run evidence bundle
   #:*ar-current-run-id*
   #:ar-note-research-artifact
   #:ar-note-evidence
   #:run-evidence-bundle
   #:project-evidence-bundle
   #:evidence-grounding-problem
   #:design-basis-items
   #:design-evidence-rows

   ;; runtime accessors
   #:ar-root
   #:ar-runs-slot
   #:ar-requests-slot
   #:ar-candidates
   #:ar-event-store
   #:ar-store
   #:ar-forgejo
   #:ar-rlm-registry
   #:ar-run-ledger
   #:ar-candidate-ledger
   #:ar-request-ledger
   #:ar-operator
   #:ar-provider
   #:ar-get-run
   #:ar-get-request
   #:ar-list-runs
   #:ar-run-pending
   #:ar-run-candidates
   #:*ar-stage-invoker*
   #:ar-stage-error
   #:ar-submit-candidate
   #:ar-candidate-transition
    #:ar-record-review
    #:ar-revision-counts
    #:ar-run-artifacts-slot
    #:ar-run-evidence-slot
    #:ar-submit-research
    #:make-ar-stage-context
   #:ensure-path-in-store%/
   #:org-roam-root
   #:org-lexical-search

   ;; forgejo adapter
   #:forgejo-client
   #:make-forgejo-client
   #:forgejo-base-url
   #:forgejo-token-env
   #:forgejo-request
   #:forgejo-get-repo
   #:forgejo-get-issue
   #:forgejo-list-issues
   #:forgejo-create-issue
   #:forgejo-update-issue
   #:forgejo-comment-issue
   #:forgejo-issue-ref
   #:forgejo-webhook-verified-p

   ;; semantic tools
   #:install-research-tools
   #:make-research-tool-registry

   ;; auto-research runtime
   #:ar-runtime
   #:make-ar-runtime
   #:ar-core
   #:ar-store
   #:ar-rlm-registry
   #:ar-forgejo
   #:process-forgejo-event
   #:run-pending
   #:promote-candidate
   #:record-research-approval))
