;;;; run-tests.lisp --- test entry point for CI and nix checkPhase.

(require :asdf)
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))

(ql:quickload :starintel-auto-research-tests :silent t)

;; deterministic webhook verification for the test API
(setf (uiop:getenv "FORGEJO_WEBHOOK_SECRET") "test-webhook-secret")

(let ((result (starintel-auto-research-tests::run-suite)))
  (unless result
    (uiop:quit 1)))
