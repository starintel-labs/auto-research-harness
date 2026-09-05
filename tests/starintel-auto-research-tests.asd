;;;; starintel-auto-research-tests.asd --- the acceptance suite.

(asdf:defsystem #:starintel-auto-research-tests
  :description "Tests for the Auto Research CL runtime"
  :author "StarIntel"
  :license "AGPL-3.0-only"
  :serial t
  :depends-on (#:fiveam
               #:alexandria
               #:bordeaux-threads
               #:cl-prolog-rlm
               #:starintel-aradr
               #:starintel-research-core
               #:starintel-auto-research
               #:starintel-auto-research-web)
  :components
  ((:module "tests"
    :components
    ((:file "package")
     (:file "rlm-tests")
     (:file "dsl-tests")
     (:file "org-tests")
     (:file "aradr-tests")
     (:file "runtime-tests")
     (:file "grounding-tests")
     (:file "web-tests")))))
