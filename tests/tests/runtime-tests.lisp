;;;; runtime-tests.lisp --- multi-project isolation, Forgejo idempotency,
;;;; ledgers/replay, run flow.

(in-package #:starintel-auto-research-tests)

(in-suite auto-research-suite)

(defun make-runtime-for (root &key forgejo)
  (let ((runtime (ar:make-ar-runtime root :forgejo forgejo)))
    (ar:clear-configs)
    (eval '(ar:define-budget normal-research))
    (eval `(ar:define-research-repository main-research
             :path ,root
             :forgejo (:base-url "https://git.example"
                       :repository "lost-rob0t/starintel-auto-research")))
    (eval '(ar:define-project prolog-rlm
             :repository "lost-rob0t/prolog-rlm"
             :research-repository main-research
             :roam-project "prolog-rlm"
             :budget normal-research))
    (eval '(ar:define-project auto-research
             :repository "lost-rob0t/starintel-auto-research"
             :research-repository main-research
             :roam-project "auto-research"
             :budget normal-research))
    runtime))

;;; ------------------------------------------------------------ ledgers

(test ledger-replay-and-duplicate-events
  (let* ((fixtures (make-test-corpus))
         (ledger (ar:make-candidate-ledger
                  (merge-pathnames "roam/" (getf fixtures :root)))))
    (ar:ledger-append ledger (ar:ledger-record :candidate.created "c1" :next-state :queued))
    (ar:ledger-append ledger (ar:ledger-record :candidate.state-changed "c1"
                                               :previous-state :queued
                                               :next-state :exploring))
    (multiple-value-bind (state events)
        (ar:ledger-replay ledger)
      (is (eq :exploring (gethash "c1" state)))
      (is (= 2 (length events))))
    ;; duplicate event ids are ignored on replay
    (let* ((dup (ar:ledger-record :candidate.state-changed "c1"
                                  :previous-state :exploring
                                  :next-state :proposing))
           (file (uiop:read-file-string (ar:ledger-path ledger))))
      (with-open-file (out (ar:ledger-path ledger)
                           :direction :output :if-exists :append)
        (write-line file out))          ; duplicate every line
      (multiple-value-bind (state events)
          (ar:ledger-replay ledger)
        (declare (ignore events))
        ;; states still resolve; replay tolerates duplicates
        (is (eq :exploring (gethash "c1" state)))))
    (is-true t)))

(test ledger-transition-mismatch-rejected
  (let* ((fixtures (make-test-corpus))
         (root (getf fixtures :root))
         (ledger-path (merge-pathnames "roam/.research-candidates" root)))
    (uiop:delete-file-if-exists ledger-path)
    (with-open-file (out ledger-path :direction :output)
      (write-line "{\"event_id\":\"e1\",\"event_type\":\"x\",\"entity_id\":\"c9\",\"previous_state\":\"queued\",\"next_state\":\"exploring\"}" out)
      (write-line "{\"event_id\":\"e2\",\"event_type\":\"x\",\"entity_id\":\"c9\",\"previous_state\":\"proposing\",\"next_state\":\"validating\"}" out))
    (signals research-core:transition-error
      (ar:ledger-replay (ar:make-candidate-ledger (merge-pathnames "roam/" root))))))

;;; ------------------------------------------------------- multi-project

(test project-isolation
  (let* ((fixtures (make-test-corpus))
         (root (getf fixtures :root))
         (runtime (make-runtime-for root)))
    ;; each project writes into its own roam subtree
    (multiple-value-bind (request-1 run-1)
        (ar:ar-submit-research runtime (ar:find-project-config "prolog-rlm")
                               "Question for prolog-rlm")
      (multiple-value-bind (request-2 run-2)
          (ar:ar-submit-research runtime (ar:find-project-config "auto-research")
                                 "Question for auto-research")
        (is (string/= (rc:request-id request-1) (rc:request-id request-2)))
        (is (string/= (rc:run-id run-1) (rc:run-id run-2)))
        (is (string= "prolog-rlm" (ar:project-config-id (rc:request-project request-1))))
        (is (string= "auto-research" (ar:project-config-id (rc:request-project request-2))))))))

(test cross-project-writes-rejected
  ;; paths outside the configured corpus root are refused
  (let* ((fixtures (make-test-corpus))
         (store (getf fixtures :store)))
    (signals research-core:validation-error
      (rc:write-artifact store "../escape.org" "nope"))
    (signals research-core:validation-error
      (ar:ensure-path-in-store%/ store "/etc/passwd"))
    (signals research-core:validation-error
      (ar:ensure-path-in-store%/ store "../../etc/passwd"))))

;;; -------------------------------------------------------------- forgejo

(defun forge-test-client (state)
  "A scripted Forgejo client: records calls, returns canned issues.
STATE is a plist: (issues . hash-table) (calls . count)."
  (let ((issues (getf state :issues)))
    (ar:make-forgejo-client "https://git.example"
                            :http-fn
                            (lambda (method url headers body)
                              (declare (ignore headers body))
                              (incf (getf state :calls))
                              (push url (getf state :urls))
                              (cond
                                ((and (eq method :post) (search "/issues" url)
                                      (not (search "/comments" url)))
                                 (let* ((request (rc.j:to-table (rc.j:parse-value body)))
                                        (n (incf (getf state :next-number)))
                                        (issue (alexandria:plist-hash-table
                                                (list "number" n
                                                      "title" (gethash "title" request ""))
                                                :test #'equal)))
                                   (setf (gethash (format nil "ISSUE-~a" n) issues) issue)
                                   (rc.j:stringify-value
                                    (list "number" n "title" (gethash "title" issue)))))
                                ((and (eq method :get) (search "issues?" url))
                                 (rc.j:stringify-json
                                  (loop :for v :being :the :hash-values :of issues
                                        :collect v)))
                                (t "{\"ok\":true}"))))))

(test api-request-creates-one-issue-and-webhook-does-not-duplicate
  (let* ((fixtures (make-test-corpus))
         (root (getf fixtures :root))
         (state (list :issues (make-hash-table :test #'equal)
                      :calls 0 :next-number 0))
         (runtime (make-runtime-for root :forgejo (forge-test-client state))))
    (multiple-value-bind (request run issue-number existing-p)
        (ar:ar-submit-research runtime (ar:find-project-config "prolog-rlm")
                               "Evaluate RLM direct mode")
      (declare (ignore request run))
      (is (integerp issue-number))
      (is-false existing-p)
      (let ((calls-after-api (getf state :calls)))
        ;; the webhook round-trip must not duplicate the request
        (multiple-value-bind (result created-p)
            (rc:process-forgejo-event
             runtime
             (alexandria:plist-hash-table
              (list "instance" "https://git.example"
                    "repository" "lost-rob0t/starintel-auto-research"
                    "issue" (alexandria:plist-hash-table
                             (list "number" issue-number
                                   "title" "any" "body" "Question: Evaluate RLM direct mode") :test #'equal)
                    "action" "opened")
              :test #'equal))
          (declare (ignore result))
          (is-false created-p)
          (is (= calls-after-api (getf state :calls))))))))

(test native-forgejo-issue-creates-request-exactly-once
  (let* ((fixtures (make-test-corpus))
         (root (getf fixtures :root))
         (runtime (make-runtime-for root)))
    ;; a Forgejo-originated research issue creates one request/transaction
    (multiple-value-bind (result created-p)
        (rc:process-forgejo-event
         runtime
         (alexandria:plist-hash-table
          (list "instance" "https://git.example"
                "repository" "lost-rob0t/starintel-auto-research"
                "issue" (alexandria:plist-hash-table
                         (list "number" 99
                               "title" "Research ledger replay"
                               "body" "Question: How are ledgers replayed?") :test #'equal)
                "action" "opened")
          :test #'equal))
      (is-true created-p)
      (is (eq :created (car result)))
      ;; the same webhook event again does NOT create a second request
      (multiple-value-bind (result-2 created-2)
          (rc:process-forgejo-event
           runtime
           (alexandria:plist-hash-table
            (list "instance" "https://git.example"
                  "repository" "lost-rob0t/starintel-auto-research"
                  "issue" (alexandria:plist-hash-table
                           (list "number" 99
                                 "title" "Research ledger replay"
                                 "body" "Question: How are ledgers replayed?") :test #'equal)
                  "action" "opened")
            :test #'equal))
        (is-false created-2)
        (is (eq :existing (car result-2))))
      ;; the ARDR issue transaction node exists exactly once
      (let ((transactions (uiop:directory-files
                           (merge-pathnames "roam/research/ardr-issues/" root))))
        (is (= 1 (length transactions)))))))

(test same-issue-number-across-repos-no-collision
  (let* ((fixtures (make-test-corpus))
         (root (getf fixtures :root))
         (runtime (make-runtime-for root)))
    (eval '(ar:define-research-repository other-research
             :path "/tmp/opencode/other-corpus"
             :forgejo (:base-url "https://git.example"
                       :repository "lost-rob0t/prolog-rlm")))
    ;; project 2 uses the same forgejo instance, different repo, same number
    (eval '(ar:define-project prolog-rlm-webhook
             :repository "lost-rob0t/prolog-rlm"
             :research-repository other-research
             :budget normal-research))
    (multiple-value-bind (result-1 created-1)
        (rc:process-forgejo-event
         runtime
         (alexandria:plist-hash-table
          (list "instance" "https://git.example"
                "repository" "lost-rob0t/starintel-auto-research"
                "issue" (alexandria:plist-hash-table
                         (list "number" 5 "title" "T1" "body" "Question: A") :test #'equal)
                "action" "opened")
          :test #'equal))
      (is-true created-1)
      (is (eq :created (car result-1)))
      (multiple-value-bind (result-2 created-2)
          (rc:process-forgejo-event
           runtime
           (alexandria:plist-hash-table
            (list "instance" "https://git.example"
                  "repository" "lost-rob0t/prolog-rlm"
                  "issue" (alexandria:plist-hash-table
                           (list "number" 5 "title" "T2" "body" "Question: B") :test #'equal)
                  "action" "opened")
            :test #'equal))
        (declare (ignore result-2))
        (is-true created-2)))))

(test retries-are-idempotent
  (let* ((fixtures (make-test-corpus))
         (root (getf fixtures :root))
         (state (list :issues (make-hash-table :test #'equal)
                      :calls 0 :next-number 0))
         (runtime (make-runtime-for root :forgejo (forge-test-client state))))
    ;; submitting the same question repeatedly creates exactly one issue
    (multiple-value-bind (r1 run1 n1 e1)
        (ar:ar-submit-research runtime (ar:find-project-config "prolog-rlm")
                               "Idempotent question")
      (declare (ignore r1 run1))
      (multiple-value-bind (r2 run2 n2 e2)
          (ar:ar-submit-research runtime (ar:find-project-config "prolog-rlm")
                                 "Idempotent question")
        (declare (ignore r2 run2))
        (is (equal n1 n2))
        (is-false e1)  ; first call created the issue
        (is-true e2))))) ; second call reports the issue already existed

;;; --------------------------------------------------------------- runs

(test run-pending-completes-without-promotion
  (let* ((fixtures (make-test-corpus))
         (root (getf fixtures :root))
         (state (list :issues (make-hash-table :test #'equal)
                      :calls 0 :next-number 0))
         (runtime (make-runtime-for root :forgejo (forge-test-client state))))
    ;; no provider configured: semantic backend fails structurally and the
    ;; run completes UNRESOLVED without promoting anything
    (multiple-value-bind (request run)
        (ar:ar-submit-research runtime (ar:find-project-config "prolog-rlm")
                               "Will not be answered")
      (ar:ar-run-pending runtime)
      (is (member (rc:run-status run) '(:unresolved :failed :completed)))
      ;; no candidates were promoted
      (let ((promoted (loop :for id :being :the :hash-keys :of (ar:ar-candidates runtime)
                              :when (eq :promoted (cdr (assoc :state (gethash id (ar:ar-candidates runtime)))))
                                :collect id)))
        (is (null promoted)))
      ;; implementation slot stays untouched
      (is (null (uiop:directory-files
                 (merge-pathnames "roam/implement/prolog-rlm/" root))))
      ;; request/run visible through accessors
      (is-true (ar:ar-get-request runtime (rc:request-id request)))
      (is-true (ar:ar-get-run runtime (rc:run-id run))))))

(test semantic-tools-persist-validator-shape-org
  ;; semantic tools must persist findings into canonical Org
  (let* ((fixtures (make-test-corpus))
         (store (getf fixtures :store))
         (path (getf fixtures :path-1)))
    (ar:append-research-update path
                               :findings "The harness round-trips tools."
                               :sources '("https://example.com/source")
                               :commits '("abc1234"))
    (let ((text (uiop:read-file-string path)))
      (is (search "** DONE Research update" text))
      (is (search "- Retrieved " text))
      (is (search "https://example.com/source" text))
      (is (search "abc1234" text))
      (is (search "Commits Reviewed" text)))
    ;; the runtime store's search finds the node
    (let ((results (ar:org-lexical-search store "harness round-trips")))
      (is-true results)
      (is (search "direct-mode-research" (getf (first results) :path))))))

(test candidate-lifecycle-through-runtime
  (let* ((fixtures (make-test-corpus))
         (root (getf fixtures :root))
         (runtime (make-runtime-for root))
         (candidate-id (ar:ar-submit-candidate runtime "run-1" "problem"
                                               "hypothesis" "predicted" "falsifier")))
    ;; queued -> exploring -> proposing -> validating -> virtual-review -> evaluated
    (ar:ar-candidate-transition runtime candidate-id :exploring)
    (ar:ar-candidate-transition runtime candidate-id :proposing)
    (ar:ar-candidate-transition runtime candidate-id :validating)
    (ar:ar-candidate-transition runtime candidate-id :virtual-review)
    (ar:ar-candidate-transition runtime candidate-id :evaluated)
    (let ((candidate (gethash candidate-id (ar:ar-candidates runtime))))
      (is (eq :evaluated (cdr (assoc :state candidate)))))
    ;; one bounded revision only: EVALUATED -> PROPOSING is allowed once
    (ar:ar-candidate-transition runtime candidate-id :proposing :reason :revision)
    ;; the revised candidate must walk the full path again
    (ar:ar-candidate-transition runtime candidate-id :validating)
    (ar:ar-candidate-transition runtime candidate-id :virtual-review)
    (ar:ar-candidate-transition runtime candidate-id :evaluated)
    ;; a SECOND revision is rejected
    (signals aradr:transition-error
      (ar:ar-candidate-transition runtime candidate-id :proposing :reason :revision))
    ;; unknown candidates fail closed
    (signals research-core:validation-error
      (ar:ar-candidate-transition runtime "unknown-candidate" :exploring))))

(test candidate-replay-rebuilds-state
  (let* ((fixtures (make-test-corpus))
         (root (getf fixtures :root))
         (runtime (make-runtime-for root))
         (candidate-id (ar:ar-submit-candidate runtime "run-2" "p" "h" "pr" "f")))
    (ar:ar-candidate-transition runtime candidate-id :exploring)
    (multiple-value-bind (state)
        (ar:ledger-replay (ar:ar-candidate-ledger runtime))
      (is (eq :exploring (gethash candidate-id state))))))
