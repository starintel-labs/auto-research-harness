;;;; dsl-tests.lisp --- project DSL: mapping, duplicates, bad references.

(in-package #:starintel-auto-research-tests)

(in-suite auto-research-suite)

(defun setup-dsl-fixtures ()
  (ar:clear-configs)
  (eval '(ar:define-budget normal-research
           :wall-clock-seconds 7200
           :input-tokens 1000000
           :output-tokens 100000
           :max-candidates 5
           :max-active-branches 3
           :max-depth 4))
  (eval '(ar:define-research-repository main-research
           :path "/tmp/opencode/arh-corpus"
           :artifact-store :org-roam
           :event-store :jsonl
           :forgejo (:base-url "https://git.example"
                     :repository "lost-rob0t/starintel-auto-research"))))

(test dsl-budget-and-repository
  (setup-dsl-fixtures)
  (is-true (ar:find-budget-profile "normal-research"))
  (let ((repo (ar:find-research-repository "main-research")))
    (is-true repo)
    (is (string= "lost-rob0t/starintel-auto-research"
                 (getf (ar:research-repository-forgejo repo) :repository)))))

(test dsl-project-maps-to-roam-and-forgejo
  (setup-dsl-fixtures)
  (eval '(ar:define-project prolog-rlm
           :repository "lost-rob0t/prolog-rlm"
           :research-repository main-research
           :method aradr
           :roam-project "prolog-rlm"
           :budget normal-research
           :rlm (:mode :direct :profile :research)
           :incoming (:forgejo-label "research" :api t)))
  (let ((project (ar:find-project-config "prolog-rlm")))
    (is-true project)
    (is (string= "prolog-rlm" (ar:project-config-roam-slug project)))
    (is (eq :direct (ar:project-config-rlm-mode project)))
    (is (string= "lost-rob0t/prolog-rlm"
                 (ar:project-config-repository project)))
    (is (string= "lost-rob0t/starintel-auto-research"
                 (getf (ar:research-repository-forgejo
                        (ar:project-config-research-repository project))
                       :repository)))))

(test dsl-duplicate-project-rejected
  (setup-dsl-fixtures)
  (eval '(ar:define-project dup-test
           :repository "org/x"
           :research-repository main-research
           :budget normal-research))
  (signals research-core:validation-error
    ;; duplicate IDs are rejected at load time
    (rc:make-runtime
     :projects (list (rc:make-project :id "same")
                     (rc:make-project :id "same"))
     :store (rc:make-jsonl-event-store
             "/tmp/opencode/dup-events.jsonl"))))

(test dsl-bad-references-rejected
  (ar:clear-configs)
  ;; unknown research repository
  (signals research-core:validation-error
    (eval '(ar:define-project bad-repo
             :repository "org/x"
             :research-repository nonexistent-repo
             :budget nil)))
  ;; missing repository entirely
  (signals research-core:validation-error
    (eval '(ar:define-project no-repo
             :research-repository nil
             :budget nil)))
  ;; missing budget reference
  (eval '(ar:define-research-repository main-research :path "/tmp/opencode/arh-corpus"))
  (signals research-core:validation-error
    (eval '(ar:define-project bad-budget
             :repository "org/x"
             :research-repository main-research
             :budget nonexistent-budget))))

(test dsl-same-issue-number-two-repos-distinct-identity
  ;; same issue number in two repositories must not collide
  (let ((ref-a (rc:make-issue-ref :forgejo "https://git.example" "org/alpha" 7))
        (ref-b (rc:make-issue-ref :forgejo "https://git.example" "org/beta" 7)))
    (is (string/= (rc:issue-ref-idempotency-key ref-a)
                  (rc:issue-ref-idempotency-key ref-b)))
    (is (string= "forgejo:https://git.example:org/alpha:7"
                 (rc:issue-ref-idempotency-key ref-a)))))
