;;;; org-tests.lisp --- Org contracts: keywords, approval, validators.

(in-package #:starintel-auto-research-tests)

(in-suite auto-research-suite)

(test org-keyword-parse-and-upsert
  (let* ((text ":PROPERTIES:
:ID:       starintel-test
:END:
#+title: Test Node
#+description: A test node.
#+status: REVIEW
#+filetags: :starintel:research:

* Objective
")
         (keywords (ar:parse-org-keywords text)))
    (is (string= "Test Node" (gethash "title" keywords)))
    (is (string= "REVIEW" (gethash "status" keywords)))
    ;; upsert replaces the first match
    (let ((updated (ar:upsert-org-keyword text "status" "RESEARCHING")))
      (is (and (search "#+status: RESEARCHING" updated) (not (search "#+status: REVIEW" updated))))
      (is (search "#+status: RESEARCHING" updated)))
    ;; upsert inserts after the last keyword when absent
    (let ((updated (ar:upsert-org-keyword text "last_modified" "2026-01-01")))
      (is (search "#+last_modified: 2026-01-01" updated))
      (is (search "#+status: REVIEW" updated)))))

(test lifecycle-is-not-approval
  ;; #+status: REVIEW does NOT mean approval_state APPROVED
  (let* ((text "#+title: N
#+description: d
#+status: REVIEW
")
         (lifecycle (ar:lifecycle-status-of text))
         (approval (ar:approval-state-of text)))
    (is (eq :review lifecycle))
    (is (eq :unmigrated approval))
    (is (not (eq lifecycle approval)))))

(test partial-approval-metadata-rejected
  ;; three of seven canonical fields is invalid, never interpreted
  (let ((text "#+title: N
#+description: d
#+status: DRAFT
#+approval_schema: prolog-rlm.research-approval.v1
#+approval_state: PENDING
#+approval_actor: someone
"))
    (signals research-core:validation-error
      (ar:approval-metadata-from-keywords (ar:parse-org-keywords text)))
    ;; the validator layer also rejects it
    (signals research-core:validation-error
      (ar:validate-approval-metadata
       (list (cons "approval_schema" ar:+approval-schema+)
             (cons "approval_state" "PENDING")
             (cons "approval_actor" "someone"))))))

(test explicit-approval-persists-correctly
  (let* ((fixtures (make-test-corpus))
         (store (getf fixtures :store))
         (root (getf fixtures :root))
         (path (getf fixtures :path-1))
         (runtime (make-instance 'ar:ar-runtime
                                 :root root
                                 :store store
                                 :rlm-registry (ar:make-research-tool-registry)
                                 :event-store (rc:make-jsonl-event-store
                                               (merge-pathnames ".ar-events.jsonl" root))))
         (text-before (uiop:read-file-string path)))
    ;; lifecycle must stay untouched by approval
    (is (eq :draft (ar:lifecycle-status-of text-before)))
    (ar:record-research-approval runtime
                                 (enough-namestring path root)
                                 "APPROVED" "test-operator"
                                 "manual test approval evidence")
    (let* ((text-after (uiop:read-file-string path))
           (keywords (ar:parse-org-keywords text-after)))
      (is (string= "APPROVED" (gethash "approval_state" keywords)))
      (is (string= "test-operator" (gethash "approval_actor" keywords)))
      ;; lifecycle is byte-for-byte unchanged
      (is (eq :draft (ar:lifecycle-status-of text-after)))
      (is (search "#+status: DRAFT" text-after))
      ;; all seven canonical fields present
      (dolist (field ar:+approval-fields+)
        (is-true (gethash field keywords) field)))))

(test pending-approval-has-none-decided-at
  (let ((rendered (ar:render-approval-keywords
                   "PENDING" "reviewer" "evidence" "abc123" "blob123")))
    (is (search "#+approval_decided_at: NONE" rendered))
    (is (search "#+approval_state: PENDING" rendered))))

(test generated-corpus-passes-canonical-validators
  ;; Generated artifacts must pass the CURRENT Auto Research validators:
  ;; scripts/validate-docs.py of the canonical checkout.
  (let* ((fixtures (make-test-corpus))
         (root (getf fixtures :root))
         (validator (merge-pathnames
                     "scripts/validate-docs.py"
                     (pathname (or (uiop:getenv "AR_CANONICAL_REPO")
                                   "/home/unseen/starintel/starintel-auto-research/")))))
    ;; an ARDR issue transaction node (created by the runtime) also validates
    (ar:write-ardr-issue-transaction (ar:make-org-roam-store root)
                                     "https://git.example"
                                     "lost-rob0t/starintel-auto-research" 42
                                     "test-project" "ARDR issue 42" "test question")
    (is (probe-file (merge-pathnames
                     "roam/research/ardr-issues/ARDR-ISSUE-42-test-project.org" root)))
    (when (probe-file validator)
      ;; the canonical validator also audits repository policy: provide a
      ;; conforming AGENTS.md and Pages workflow in the temp corpus
      (write-canonical-validator-fixtures root)
      (multiple-value-bind (output errors exit-code)
          (uiop:run-program (list "python3" (namestring validator)
                                  "--root" (namestring root))
                            :output :string :error-output :string
                            :ignore-error-status t)
        (declare (ignore output))
        (when (/= exit-code 0)
          (format *error-output* "~a~%" errors))
        ;; The canonical validator enforces approval tables, changelogs,
        ;; glossary, and metadata on every substantive document class.
        ;; Org nodes produced by this runtime must satisfy them.
        (is (= 0 exit-code))))))

(defun write-canonical-validator-fixtures (root)
  "Minimal AGENTS.md and CI workflow satisfying validate-docs.py policy."
  (let* ((commands '("python3 scripts/sync.py"
                     "python3 scripts/sync.py --check"
                     "python3 scripts/validate-docs.py"
                     "bash scripts/publish-pages"
                     "python3 scripts/check-pages-links.py _site"))
         (agents (format nil "# Test corpus agent instructions~%

The canonical source synchronization and complete-site validation sequence is:

```bash
python3 scripts/sync.py
python3 scripts/sync.py --check
python3 scripts/validate-docs.py
bash scripts/publish-pages
python3 scripts/check-pages-links.py _site
```

Rules: never hand-edit generated output; never claim a check passed; locate every nested `agents.md`; the public site is auto-research.starintel.actor.
")))
    (with-open-file (out (merge-pathnames "AGENTS.md" root)
                         :direction :output :external-format :utf-8)
      (write-string agents out))
    (ensure-directories-exist (merge-pathnames ".github/workflows/" root))
    (with-open-file (out (merge-pathnames ".github/workflows/pages.yml" root)
                         :direction :output :external-format :utf-8)
      (format out "jobs:~%  pages:~%    steps:~%")
      (dolist (c commands)
        (format out "      - run: ~a~%" c)))))
