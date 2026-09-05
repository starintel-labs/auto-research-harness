;;;; grounding-tests.lisp --- research grounding, design provenance and
;;;; structural validation, promotion gating, project routing/identity,
;;;; prompt profiles, and source-repository separation.
;;;;
;;;; Includes the regression fixture for the failure that produced the
;;;; generic "Project auto-research / Actors: Researcher, System,
;;;; Reviewer" design for a Star-Lang investigation: the fixed harness
;;;; must be unable to take that path.

(in-package #:starintel-auto-research-tests)

(in-suite auto-research-suite)

;;; ------------------------------------------------- prompt profiles (4)

(test prompt-profile-files-resolve-in-precedence-order
  (let* ((fixtures (make-test-corpus))
         (root (getf fixtures :root))
         (runtime (make-runtime-for root))
         (project (ar:find-project-config "star-lang")))
    (declare (ignore root))
    ;; general profile context + stage file, context first
    (let ((files (ar:project-prompt-files runtime project :design)))
      (is (= 2 (length files)))
      (is (search "context.md" (namestring (first files))))
      (is (search "design.md" (namestring (second files)))))
    ;; stages without a dedicated file get the profile context only
    (let ((files (ar:project-prompt-files runtime project :research)))
      (is (= 1 (length files)))
      (is (search "context.md" (namestring (first files)))))
    (let ((context (ar:project-prompt-context runtime project :design)))
      (is (search "star-lang investigates the Common Lisp star-lang repository." context))
      (is (search "Design decisions must trace to recorded star-lang research evidence." context))
      ;; explicit precedence: general context precedes stage content
      (is (< (search "star-lang investigates" context)
             (search "Design decisions must trace" context))))
    ;; missing files are skipped: a profile without files contributes nothing
    (is-false (ar:project-prompt-context
               runtime (ar:find-project-config "prolog-rlm") :design))))

(test prompt-profile-reaches-stage-context
  (let* ((fixtures (make-test-corpus))
         (root (getf fixtures :root))
         (runtime (make-runtime-for root)))
    (declare (ignore root))
    (multiple-value-bind (request run)
        (ar:ar-submit-research runtime (ar:find-project-config "star-lang")
                               "Who owns the .star parser?")
      (let ((context (rlm:context-text
                      (ar:make-ar-stage-context runtime run request
                                                (ar:find-project-config "star-lang")
                                                :research))))
        (is (search "star-lang investigates the Common Lisp star-lang repository." context))
        ;; profile content is part of the stage context, not the question
        (is (search "question: Who owns the .star parser?" context))))))

;;; ------------------------------------- project routing and identity (1-3)

(test unknown-project-fails-closed
  (let* ((fixtures (make-test-corpus))
         (root (getf fixtures :root))
         (runtime (make-runtime-for root)))
    (declare (ignore root))
    ;; strict resolution fails; it never falls back to a default project
    (signals ar:unknown-project-error
      (ar:find-project-config-or-fail "star-lanng"))
    (is-false (ar:find-project-config "star-lanng"))
    ;; known projects still resolve
    (is-true (ar:find-project-config-or-fail "star-lang"))
    (is-true runtime)))

(test star-lang-file-input-preserves-project-identity
  ;; the regression invocation: `research star-lang prompts/star-lang/bounder.md`
  ;; submitted the literal words under auto-research. The fixed contract
  ;; resolves the project explicitly and records the file input.
  (let* ((fixtures (make-test-corpus))
         (root (getf fixtures :root))
         (runtime (make-runtime-for root))
         (question (uiop:read-file-string
                    (first (ar:project-prompt-files
                            runtime (ar:find-project-config "star-lang") :design)))))
    (declare (ignore root))
    (multiple-value-bind (request run)
        (ar:ar-submit-research runtime (ar:find-project-config "star-lang")
                               question
                               :input-kind :file
                               :input-path "prompts/star-lang/bounder.md")
      ;; the run is for star-lang and can never become auto-research
      (is (string= "star-lang"
                   (ar:project-config-id (rc:request-project request))))
      (is (string= "star-lang"
                   (ar:project-config-id (rc:run-project run))))
      (is (eq :file (rc:request-input-kind request)))
      (is (string= "prompts/star-lang/bounder.md" (rc:request-input-path request)))
      (is (eq :queued (rc:run-status run)))
      ;; identity is immutable: the request keeps its project object
      (is (eq request (ar:ar-get-request runtime (rc:request-id request)))))))

(test run-walk-keeps-its-own-project-subtree
  (let* ((fixtures (make-test-corpus))
         (root (getf fixtures :root))
         (runtime (make-runtime-for root))
         (finding-path (enough-namestring (getf fixtures :path-2)
                                          (ar:org-roam-root (ar:ar-store runtime)))))
    (multiple-value-bind (request run)
        (ar:ar-submit-research runtime (ar:find-project-config "star-lang")
                               "Star-Lang parser ownership")
      (declare (ignore request))
      (let ((ar:*ar-stage-invoker* (stage-walk-stub-invoker finding-path :with-evidence t)))
        (ar:ar-run-pending runtime))
      (is (eq :completed (rc:run-status run)))
      ;; the design node lives in the STAR-LANG subtree, never auto-research
      (is (search "star-lang"
                  (namestring (ar:design-node-path (ar:ar-store runtime)
                                                   "star-lang" (rc:run-id run)))))
      (is-true (probe-file (ar:design-node-path (ar:ar-store runtime)
                                                "star-lang" (rc:run-id run)))))))

;;; ------------------------------------------- evidence bundle (5) and gate

(test evidence-bundle-accumulates-recorded-run-evidence
  (let* ((fixtures (make-test-corpus))
         (root (getf fixtures :root))
         (runtime (make-runtime-for root))
         (finding-path (enough-namestring (getf fixtures :path-1)
                                          (ar:org-roam-root (ar:ar-store runtime)))))
    (multiple-value-bind (request run)
        (ar:ar-submit-research runtime (ar:find-project-config "star-lang")
                               "Parser ownership")
      (declare (ignore request root))
      (let ((ar:*ar-current-run-id* (rc:run-id run))
            (registry (ar:ar-rlm-registry runtime)))
        (rlm:invoke-registry-tool registry "record-finding"
                                  (alexandria:plist-hash-table
                                   (list "path" finding-path
                                         "finding" "star.lang.parser owns parse-star-file")
                                   :test #'equal))
        (rlm:invoke-registry-tool registry "record-source"
                                  (alexandria:plist-hash-table
                                   (list "path" finding-path
                                         "source" "src/starlang/parser.lisp")
                                   :test #'equal))
        (rlm:invoke-registry-tool registry "submit-candidate"
                                  (alexandria:plist-hash-table
                                   (list "run_id" (rc:run-id run)
                                         "problem" "parser ownership"
                                         "hypothesis" "the CL parser is canonical"
                                         "predicted_result" "manifest names star.lang.parser"
                                         "falsifier" "no parser system in the manifest"
                                         "evidence" "src/starlang/parser.lisp"
                                         "acceptance_criteria" "manifest check passes"
                                         "rejected_alternatives" "gserver-owned parser")
                                   :test #'equal)))
      (let ((bundle (ar:run-evidence-bundle runtime run)))
        (is (string= "star-lang" (getf bundle :project-id)))
        (is (= 1 (length (getf bundle :research-artifacts))))
        (is (search "star.lang.parser owns parse-star-file"
                    (getf (first (getf bundle :findings)) :text)))
        (is (string= "src/starlang/parser.lisp"
                     (getf (first (getf bundle :sources)) :text)))
        (is (= 1 (length (getf bundle :candidates))))
        (is (member "src/starlang/parser.lisp"
                    (getf (first (getf bundle :candidates)) :evidence)
                    :test #'string=))))))

(test evidence-projection-is-bounded-and-semantic
  ;; the projection stays within budget even with oversized entries
  (let ((text (ar:project-evidence-bundle
               (list :run-id "run-y" :project-id "p" :question "q"
                     :research-artifacts nil
                     :findings (list (list :kind :finding :path "p"
                                           :text (make-string 20000 :initial-element #\x)))
                     :sources nil :counterevidence nil
                     :unresolved-questions nil
                     :repository-observations nil
                     :candidates nil)
               :max-bytes 2048)))
    (is (<= (length text) 2100))
    (is (search "evidence projection truncated" text))))

;;; --------------------------------- NO EVIDENCE -> NO DESIGN (6) + fixture

(defun unevidenced-stub-invoker (hypothesis)
  "Scripted semantic stages for an UNEVIDENCED run: reviews are recorded
(their absence would fail the review stage instead of the design gate),
but no research finding, source, or counterevidence is ever recorded and
the candidate cites no evidence — the shape that produced the generic
auto-research design for a Star-Lang question."
  (lambda (runtime run request project stage)
    (declare (ignore request project))
    (let ((registry (ar:ar-rlm-registry runtime)))
      (when (eq stage :research)
        (rlm:invoke-registry-tool
         registry "submit-candidate"
         (alexandria:plist-hash-table
          (list "run_id" (rc:run-id run)
                "problem" "actor materialization ownership"
                "hypothesis" hypothesis
                "predicted_result" "cleaner materialization"
                "falsifier" "materialization stays duplicated")
          :test #'equal)))
      (when (eq stage :adversarial-review)
        (dolist (candidate (ar:ar-run-candidates runtime (rc:run-id run)))
          (rlm:invoke-registry-tool registry "record-review"
                                    (alexandria:plist-hash-table
                                     (list "candidate_id" (cdr (assoc :id candidate))
                                           "reviewer" "adversarial-reviewer"
                                           "evidence" 2 "architecture" 2
                                           "novelty" 2 "feasibility" 2
                                           "testability" 2 "leverage" 2
                                           "scope" 2)
                                     :test #'equal)))))
    :succeeded))

(test star-lang-misroute-regression-fixture
  ;; THE FIXTURE: an unevidenced Star-Lang investigation used to reach
  ;; DESIGN and emit "Actors: Researcher / System / Reviewer" under the
  ;; auto-research project. The fixed harness must be unable to take
  ;; that path: NO EVIDENCE -> NO DESIGN blocks the walk at :design,
  ;; no design node is written, and nothing is promoted.
  (let* ((fixtures (make-test-corpus))
         (root (getf fixtures :root))
         (runtime (make-runtime-for root)))
    (declare (ignore root))
    (multiple-value-bind (request run)
        (ar:ar-submit-research runtime (ar:find-project-config "star-lang")
                               "Transitional runtime duplication in actor materialization")
      ;; the investigation belongs to star-lang, not auto-research
      (is (string= "star-lang"
                   (ar:project-config-id (rc:request-project request))))
      (let ((ar:*ar-stage-invoker*
              (unevidenced-stub-invoker
               "the system should be extensible; actors: Researcher, System, Reviewer")))
        (ar:ar-run-pending runtime))
      (is (eq :unresolved (rc:run-status run)))
      ;; the walk stopped at design: research and reviews ran, design never did
      (is (equal '("request" "research" "adversarial-review" "analysis" "design")
                 (stage-event-order runtime (rc:run-id run))))
      (is-true (stage-event-present-p runtime (rc:run-id run) :stage.blocked))
      (let ((blocked (first (rc:replay-events (ar:ar-event-store runtime)
                                              :entity-id (rc:run-id run)
                                              :event-types '(:stage.blocked)))))
        (is (search "no-evidence-no-design"
                    (gethash "error" (rc:event-payload blocked)))))
      ;; no design artifact exists for this run in ANY project subtree
      (is-false (probe-file (ar:design-node-path (ar:ar-store runtime)
                                                 "star-lang" (rc:run-id run))))
      (is-false (probe-file (ar:design-node-path (ar:ar-store runtime)
                                                 "auto-research" (rc:run-id run))))
      ;; nothing was promoted
      (is-false (stage-event-present-p runtime (rc:run-id run) :promotion.recorded)))))

(test no-evidence-no-design-names-the-missing-basis
  (let* ((fixtures (make-test-corpus))
         (root (getf fixtures :root))
         (runtime (make-runtime-for root)))
    (declare (ignore root))
    (multiple-value-bind (request run)
        (ar:ar-submit-research runtime (ar:find-project-config "star-lang") "q")
      (declare (ignore request))
      ;; no artifact, no finding, no candidate
      (is (search "no research artifact"
                  (ar:evidence-grounding-problem runtime run)))
      ;; a finding without its artifact is still insufficient
      (let ((ar:*ar-current-run-id* (rc:run-id run)))
        (ar:ar-note-evidence runtime (rc:run-id run) :finding nil "orphan finding"))
      (is (search "no research artifact"
                  (ar:evidence-grounding-problem runtime run)))
      ;; an artifact plus a recorded finding is the minimum basis
      (ar:ar-note-research-artifact runtime (rc:run-id run) (getf fixtures :path-1))
      (is-false (ar:evidence-grounding-problem runtime run))))
  ;; a fresh run with an artifact but no recorded evidence at all
  (let* ((fixtures (make-test-corpus))
         (root (getf fixtures :root))
         (runtime (make-runtime-for root)))
    (declare (ignore root))
    (multiple-value-bind (request run)
        (ar:ar-submit-research runtime (ar:find-project-config "star-lang") "q")
      (declare (ignore request))
      (ar:ar-note-research-artifact runtime (rc:run-id run) (getf fixtures :path-1))
      (is (search "no finding, source, or counterevidence"
                  (ar:evidence-grounding-problem runtime run)))
      ;; a source without findings or candidates is not a research
      ;; conclusion either
      (let ((ar:*ar-current-run-id* (rc:run-id run)))
        (ar:ar-note-evidence runtime (rc:run-id run) :source
                             (getf fixtures :path-1) "https://example.org/x"))
      (is (search "no candidate or explicit research conclusion"
                  (ar:evidence-grounding-problem runtime run)))
      ;; a candidate closes the gap
      (ar:ar-submit-candidate runtime (rc:run-id run) "p" "h" "pr" "f")
      (is-false (ar:evidence-grounding-problem runtime run)))))

(test design-context-carries-evidence-not-just-candidate-prose
  (let* ((fixtures (make-test-corpus))
         (root (getf fixtures :root))
         (runtime (make-runtime-for root))
         (finding-path (enough-namestring (getf fixtures :path-1)
                                          (ar:org-roam-root (ar:ar-store runtime)))))
    (multiple-value-bind (request run)
        (ar:ar-submit-research runtime (ar:find-project-config "star-lang")
                               "Parser ownership")
      (declare (ignore request root))
      (let ((ar:*ar-current-run-id* (rc:run-id run)))
        (ar:ar-note-research-artifact runtime (rc:run-id run) finding-path)
        (ar:ar-note-evidence runtime (rc:run-id run) :finding finding-path
                          "star.lang.parser owns parse-star-file"))
      (ar:ar-submit-candidate runtime (rc:run-id run) "problem" "h"
                              "predicted" "falsifier"
                              nil :evidence "src/starlang/parser.lisp")
      (let ((context (rlm:context-text
                      (ar:make-ar-stage-context runtime run request
                                                (ar:find-project-config "star-lang")
                                                :design))))
        ;; the design context contains the semantic evidence...
        (is (search "star.lang.parser owns parse-star-file" context))
        (is (search "src/starlang/parser.lisp" context))
        (is (search "research artifacts:" context))
        (is (search "roam/research/" context))
        ;; ...and stays within the context budget
        (is (<= (rlm:context-bytes
                 (ar:make-ar-stage-context runtime run request
                                           (ar:find-project-config "star-lang") :design))
                8192))))))

;;; ------------------------------- design provenance and validation (7, 8)

(defun record-design-through-tool (runtime run design-text)
  "Record the design note for RUN through the real registry tool; returns
the tool result (a dict with the node path)."
  (let ((ar:*ar-current-run-id* (rc:run-id run)))
    (rlm:invoke-registry-tool
     (ar:ar-rlm-registry runtime) "record-design-note"
     (alexandria:plist-hash-table
      (list "run_id" (rc:run-id run) "design" design-text)
      :test #'equal))))

(test design-node-carries-research-basis-provenance
  (let* ((fixtures (make-test-corpus))
         (root (getf fixtures :root))
         (runtime (make-runtime-for root))
         (finding-path (enough-namestring (getf fixtures :path-2)
                                          (ar:org-roam-root (ar:ar-store runtime)))))
    (multiple-value-bind (request run)
        (ar:ar-submit-research runtime (ar:find-project-config "star-lang")
                               "Parser ownership")
      (declare (ignore request root))
      (let ((ar:*ar-current-run-id* (rc:run-id run)))
        (ar:ar-note-research-artifact runtime (rc:run-id run) finding-path)
        (ar:ar-note-evidence runtime (rc:run-id run) :finding finding-path
                          "the parser is the canonical .star reader"))
      (ar:ar-submit-candidate runtime (rc:run-id run) "problem" "h" "pr" "f"
                              nil :evidence "src/starlang/parser.lisp")
      (let ((result (record-design-through-tool
                     runtime run
                     "Decision: keep star.lang.parser canonical (evidence: parser findings).
Inference: the prototype may later wrap it, but no evidence yet supports moving it.")))
        (let* ((relative (gethash "path" result))
               (text (uiop:read-file-string
                      (merge-pathnames relative (ar:org-roam-root (ar:ar-store runtime))))))
          ;; provenance sections exist and point at the research basis
          (is (search "* Research Basis" text))
          (is (search "roam/research/" text))
          (is (search "* Evidence Used" text))
          (is (search "| Claim | Evidence | Kind |" text))
          (is (search "the parser is the canonical .star reader" text))
          (is (search "src/starlang/parser.lisp" text))
          ;; the approval table requires the research basis (PENDING),
          ;; never NOT APPLICABLE, for a research-derived design
          (is (search "| Research basis | Research reviewer | PENDING |" text))
          (is-false (search "Research basis | Research reviewer | NOT APPLICABLE" text))
          ;; the model text cannot forge canonical provenance sections
          (is (= 1 (length (ppcre:all-matches-as-strings
                            "(?m)^\\* Research Basis$" text)))))))))

(test design-validation-rejects-missing-basis-and-empty-design
  (let* ((fixtures (make-test-corpus))
         (root (getf fixtures :root))
         (store (getf fixtures :store))
         (runtime (make-runtime-for root))
         (finding-path (enough-namestring (getf fixtures :path-1)
                                          (ar:org-roam-root (ar:ar-store runtime)))))
    (multiple-value-bind (request run)
        (ar:ar-submit-research runtime (ar:find-project-config "star-lang") "q")
      (declare (ignore request))
      ;; a design file that merely exists would not be enough anyway:
      ;; before anything is written the node is missing -> invalid
      (multiple-value-bind (valid reasons)
          (ar:validate-design-node store "star-lang" (rc:run-id run))
        (is-false valid)
        (is (search "design node missing" (first reasons))))
      ;; record evidence and write the canonical design: valid
      (let ((ar:*ar-current-run-id* (rc:run-id run)))
        (ar:ar-note-research-artifact runtime (rc:run-id run) finding-path)
        (ar:ar-note-evidence runtime (rc:run-id run) :finding finding-path "a finding"))
      (ar:ar-submit-candidate runtime (rc:run-id run) "p" "h" "pr" "f")
      (record-design-through-tool runtime run "A grounded design body.")
      (multiple-value-bind (valid reasons)
          (ar:validate-design-node store "star-lang" (rc:run-id run))
        (is-true valid)
        (is-false reasons))
      ;; tampering: remove the Research Basis section -> invalid
      (let* ((path (ar:design-node-path store "star-lang" (rc:run-id run)))
             (text (uiop:read-file-string path))
             (start (search "* Research Basis" text))
             (end (search "* Evidence Used" text)))
        (uiop:with-output-file (out path :if-exists :supersede)
          (write-string (concatenate 'string
                                     (subseq text 0 start)
                                     (subseq text end))
                        out)))
      (multiple-value-bind (valid reasons)
          (ar:validate-design-node store "star-lang" (rc:run-id run))
        (is-false valid)
        (is (member "research basis section missing" reasons :test #'string=)))
      ;; a dangling research reference -> invalid
      (let* ((path (ar:design-node-path store "star-lang" (rc:run-id run)))
             (current (uiop:read-file-string path)))
        (uiop:with-output-file (out path :if-exists :supersede)
          (write-string (concatenate 'string
                                     current
                                     "
* Research Basis

- roam/research/star-lang/does-not-exist.org (Ghost)
")
                        out)))
      (multiple-value-bind (valid reasons)
          (ar:validate-design-node store "star-lang" (rc:run-id run))
        (is-false valid)
        (is (find "unresolved research basis reference" reasons
                  :test (lambda (needle s)
                          (search needle (princ-to-string s)))))))))

;;; -------------------------------- promotion requires design validity (9)

(test promotion-requires-structurally-valid-design
  (let* ((fixtures (make-test-corpus))
         (root (getf fixtures :root))
         (runtime (make-runtime-for root))
         (finding-path (enough-namestring (getf fixtures :path-1)
                                          (ar:org-roam-root (ar:ar-store runtime)))))
    (multiple-value-bind (request run)
        (ar:ar-submit-research runtime (ar:find-project-config "star-lang")
                               "Parser ownership")
      (declare (ignore request))
      (let ((ar:*ar-stage-invoker*
              (stage-walk-stub-invoker finding-path :with-evidence t)))
        (ar:ar-run-pending runtime))
      (is (eq :completed (rc:run-status run)))
      (let ((candidate (first (ar:ar-run-candidates runtime (rc:run-id run))))
            (path (ar:design-node-path (ar:ar-store runtime) "star-lang"
                                       (rc:run-id run))))
        (is (eq :promoted (cdr (assoc :state candidate))))
        ;; regression guard: corrupt the design's provenance, then try to
        ;; promote ANOTHER candidate of the same run through the runtime
        ;; path — design validity is mandatory for promotion
        (let ((text (uiop:read-file-string path)))
          (with-open-file (out path :direction :output :if-exists :supersede)
            (write-string (ppcre:regex-replace-all "\\* Research Basis\\s*" text "") out)))
        (let ((second (ar:ar-submit-candidate runtime (rc:run-id run) "p2" "h2"
                                              "pr2" "f2" nil
                                              :evidence "src/starlang/parser.lisp"
                                              :acceptance-criteria "criteria"
                                              :rejected-alternatives "alternative")))
          (dolist (transition '(:exploring :proposing :validating
                                :virtual-review :evaluated))
            (ar:ar-candidate-transition runtime second transition))
          (ar:ar-record-review runtime second
                               (alexandria:plist-hash-table
                                (list "reviewer" "adversarial-reviewer"
                                      "evidence" 5 "architecture" 5)
                                :test #'equal))
          (signals aradr:promotion-denied
            (ar:promote-candidate runtime second
                                  (list (cons :evidence 5) (cons :architecture 5))
                                  (list :evidence-min 4 :architecture-min 4)))))))

;;; ------------------------- source repository vs research corpus (10)

(test repository-tools-read-the-target-source-repository
  (let* ((fixtures (make-test-corpus))
         (root (getf fixtures :root))
         (runtime (make-runtime-for root))
         (registry (ar:ar-rlm-registry runtime)))
    (multiple-value-bind (request run)
        (ar:ar-submit-research runtime (ar:find-project-config "star-lang")
                               "Parser ownership")
      (declare (ignore request))
      (let ((ar:*ar-current-run-id* (rc:run-id run)))
        ;; reads hit the configured SOURCE repository, not the corpus
        (is (search "star-lang fixture repository"
                    (rlm:invoke-registry-tool
                     registry "repository-read"
                     (alexandria:plist-hash-table
                      (list "path" "README.md") :test #'equal))))
        ;; corpus paths are NOT readable through the repository tool
        ;; (tool validation errors are returned as model-readable dicts)
        (is (search "outside the configured source repository"
                    (gethash "error"
                             (rlm:invoke-registry-tool
                              registry "repository-read"
                              (alexandria:plist-hash-table
                               (list "path" "roam/research/star-lang/direct-mode-research.org")
                               :test #'equal))
                             "")))
        ;; traversal is rejected
        (is (search "outside the configured source repository"
                    (gethash "error"
                             (rlm:invoke-registry-tool
                              registry "repository-read"
                              (alexandria:plist-hash-table
                               (list "path" "../.env") :test #'equal))
                             "")))
        ;; bounded listing and search over the target repository
        (let ((listing (rlm:invoke-registry-tool
                        registry "repository-list"
                        (alexandria:plist-hash-table (list "path" "") :test #'equal))))
          (is (string= "lost-rob0t/star-lang" (gethash "repository" listing)))
          (is (search "README.md" (princ-to-string (gethash "files" listing)))))
        (let ((found (rlm:invoke-registry-tool
                      registry "repository-search"
                      (alexandria:plist-hash-table
                       (list "query" "parse-star-file") :test #'equal))))
          (is (search "src/parser.lisp"
                      (princ-to-string (gethash "matches" found)))))
        ;; provenance: repository observations land in the evidence bundle
        (let ((observations (getf (ar:run-evidence-bundle runtime run)
                                  :repository-observations)))
          (is (plusp (length observations)))
          (is (search "repository-read" (getf (first observations) :text)))
          (is (search "lost-rob0t/star-lang" (getf (first observations) :text))))))
      ;; a project without a configured source repository fails closed:
      ;; repository tools never fall back to the research corpus
      (multiple-value-bind (prolog-request prolog-run)
          (ar:ar-submit-research runtime (ar:find-project-config "prolog-rlm") "q")
        (declare (ignore prolog-request))
        (let ((ar:*ar-current-run-id* (rc:run-id prolog-run)))
          (is (search "configures none"
                      (gethash "error"
                               (rlm:invoke-registry-tool
                                registry "repository-read"
                                (alexandria:plist-hash-table
                                 (list "path" "README.md") :test #'equal))
                               ""))))))))
