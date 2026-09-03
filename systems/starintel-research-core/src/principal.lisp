;;;; principal.lisp --- capability vocabulary and enforcement.
;;;;
;;;; Principals are authenticated callers with explicit capabilities.
;;;; Authentication failure and authorization failure are distinct
;;;; conditions: an unknown token is authentication-error; a valid
;;;; principal lacking a capability is authorization-error.

(in-package #:research-core)

(defparameter +known-capabilities+
  '(:|research:read| :|research:submit| :|research:cancel|
    :|design:approve| :|research:approve|
    :|runtime:inspect| :|runtime:admin|)
  "Canonical capability vocabulary for v1.")

(defun known-capabilities () +known-capabilities+)

(defun capability-name (capability)
  "Validate and return the canonical keyword for CAPABILITY."
  (if (member capability +known-capabilities+)
      capability
      (error 'validation-error
             :message (format nil "unknown capability: ~a" capability))))

(defgeneric principal-allowed-p (principal capability)
  (:documentation "T when PRINCIPAL has been granted CAPABILITY."))

(defmethod principal-allowed-p ((principal principal) capability)
  (member capability (principal-capabilities principal) :test #'eq))

(defun check-capability (principal capability)
  "Fail closed: raise authorization-error unless PRINCIPAL holds CAPABILITY."
  (unless principal
    (error 'authentication-error :message "no authenticated principal"))
  (unless (principal-allowed-p principal capability)
    (error 'authorization-error
           :principal (principal-id principal)
           :capability capability))
  t)
