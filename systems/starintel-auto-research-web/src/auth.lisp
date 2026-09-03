;;;; auth.lisp --- bearer-token principals with explicit capabilities.
;;;;
;;;; Authentication failure (unknown token) and authorization failure
;;;; (missing capability) are distinct conditions. Tokens come from the
;;;; trusted init configuration; never from LLM output or issue bodies.

(in-package #:starintel-auto-research-web)

(defparameter *auth-tokens* (make-hash-table :test #'equal)
  "Maps bearer token -> research-core:principal. Populated only by trusted
init configuration (configure-principal).")

(defun configure-principal (token principal)
  (setf (gethash token *auth-tokens*) principal))

(defun clear-principals ()
  (setf *auth-tokens* (make-hash-table :test #'equal)))

(define-condition web-error (error)
  ((status :initarg :status :reader web-error-status)
   (message :initarg :message :reader web-error-message))
  (:report (lambda (c s)
             (format s "~a: ~a" (web-error-status c) (web-error-message c)))))

(defun authenticate ()
  "Authenticate the current request. Returns the principal or raises
authentication-error (unknown/missing token)."
  (let* ((auth (hunchentoot:header-in* :authorization))
         (token (and auth (ppcre:register-groups-bind (tok)
                            ("^Bearer\\s+(.+)$" auth)
                          tok))))
    (unless token
      (error 'rc:authentication-error :message "missing bearer token"))
    (let ((principal (gethash token *auth-tokens*)))
      (unless principal
        (error 'rc:authentication-error :message "unknown token"))
      principal)))

(defun require-capability (capability)
  "Authenticate, then enforce CAPABILITY (authorization)."
  (let ((principal (authenticate)))
    (rc:check-capability principal capability)
    principal))
