;;;; package.lisp --- test suite package and runner.

(defpackage #:starintel-auto-research-tests
  (:use #:cl #:fiveam)
  (:local-nicknames (:rlm :rlm)
                    (:aradr :starintel-aradr)
                    (:rc :research-core)
                    (:ar :starintel-auto-research)
                    (:web :starintel-auto-research-web))
  (:export #:run-suite
           #:*test-root*
           #:*fake-transport*))

(in-package #:starintel-auto-research-tests)

(def-suite* auto-research-suite :description "Auto Research CL runtime acceptance suite")

(defun run-suite ()
  (run! 'auto-research-suite))

;;; Shared fixtures ------------------------------------------------------

(defparameter *test-root* nil
  "Per-suite temp corpus root.")

(defparameter *fake-transport* nil
  "Scripted transport speaking the bridge protocol via tests/fake_bridge.pl.")

(defun fake-transport ()
  (unless *fake-transport*
    (let ((bridge (merge-pathnames "fake_bridge.pl"
                                   (asdf:system-source-directory
                                    :starintel-auto-research-tests))))
      (setf *fake-transport*
            (rlm:make-subprocess-transport
             :swipl (find-prolog-rlm-swipl)
             :bridge (namestring (truename bridge))))))
  *fake-transport*)

(defun make-test-corpus ()
  "A fresh temp corpus with two research nodes."
  (let* ((root (uiop:merge-pathnames*
                (format nil "arh-test-~a-~a/" (get-universal-time) (random 1000000))
                (uiop:temporary-directory)))
         (store (ar:make-org-roam-store root)))
    (ar:ensure-roam-layout root)
    (let ((path-1 (ar:write-research-node store "prolog-rlm"
                                          "Direct Mode Research"
                                          :description "Evaluate direct mode."
                                          :state :draft))
          (path-2 (ar:write-research-node store "auto-research"
                                          "Ledger Replay Research"
                                          :description "Replay of run ledgers."
                                          :state :draft)))
      (list :root root :store store :path-1 path-1 :path-2 path-2))))

(defun find-prolog-rlm-swipl ()
  (or (uiop:getenv "PROLOG_RLM_SWIPL")
      "swipl"))
