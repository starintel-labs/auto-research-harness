(require :asdf)
(dolist (system (list :cl-prolog-rlm
                      :starintel-aradr
                      :starintel-research-core
                      :starintel-auto-research
                      :starintel-auto-research-web))
  (asdf:load-system system)
  (format t "~&loaded ~a~%" system))
(format t "~&AUTO-RESEARCH-SYSTEMS-LOADED~%")