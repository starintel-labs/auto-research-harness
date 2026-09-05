;;;; domain.lisp --- first-class domain objects.
;;;;
;;;; These are the reusable concepts shared by every caller. The run and
;;;; candidate shapes follow the approved STAR-RESEARCH-PIPELINE-001
;;;; contract; serialization belongs to the adapters, not this layer.

(in-package #:research-core)

(define-condition core-error (error)
  ((message :initarg :message :reader core-error-message))
  (:report (lambda (c s) (format s "core error: ~a" (core-error-message c)))))

(define-condition validation-error (core-error) ()
  (:report (lambda (c s) (format s "validation error: ~a" (core-error-message c)))))

(define-condition transition-error (core-error)
  ((from :initarg :from :reader transition-error-from)
   (to :initarg :to :reader transition-error-to))
  (:report (lambda (c s)
             (format s "invalid transition ~a -> ~a: ~a"
                     (transition-error-from c)
                     (transition-error-to c)
                     (core-error-message c)))))

(define-condition authorization-error (core-error)
  ((principal :initarg :principal :reader authorization-error-principal)
   (capability :initarg :capability :reader authorization-error-capability))
  (:report (lambda (c s)
             (format s "authorization failed: principal ~a lacks ~a"
                     (authorization-error-principal c)
                     (authorization-error-capability c)))))

(define-condition authentication-error (core-error) ()
  (:report (lambda (c s) (format s "authentication failed: ~a" (core-error-message c)))))

;;; ---------------------------------------------------------------- budget

(defclass budget ()
  ((wall-clock-seconds :initarg :wall-clock-seconds :initform 7200
                       :reader budget-wall-clock-seconds)
   (input-tokens :initarg :input-tokens :initform 1000000
                 :reader budget-input-tokens)
   (output-tokens :initarg :output-tokens :initform 100000
                  :reader budget-output-tokens)
   (max-candidates :initarg :max-candidates :initform 5
                   :reader budget-max-candidates)
   (max-active-branches :initarg :max-active-branches :initform 3
                        :reader budget-max-active-branches)
   (max-depth :initarg :max-depth :initform 4
              :reader budget-max-depth)))

(defun make-budget (&key (wall-clock-seconds 7200) (input-tokens 1000000)
                      (output-tokens 100000) (max-candidates 5)
                      (max-active-branches 3) (max-depth 4))
  (make-instance 'budget
                 :wall-clock-seconds wall-clock-seconds
                 :input-tokens input-tokens
                 :output-tokens output-tokens
                 :max-candidates max-candidates
                 :max-active-branches max-active-branches
                 :max-depth max-depth))

;;; ------------------------------------------------------------- principal

(defclass principal ()
  ((id :initarg :id :reader principal-id :type string)
   (capabilities :initarg :capabilities :initform nil
                 :reader principal-capabilities
                 :type list)))

(defun make-principal (id capabilities)
  (make-instance 'principal :id id :capabilities capabilities))

;;; ------------------------------------------------------------ issue refs

(defclass issue-ref ()
  ((kind :initarg :kind :reader issue-ref-kind)                 ; e.g. :forgejo
   (instance :initarg :instance :reader issue-ref-instance)     ; e.g. "https://git.example"
   (repository :initarg :repository :reader issue-ref-repository)
   (number :initarg :number :reader issue-ref-number :type integer)))

(defun make-issue-ref (kind instance repository number)
  "Build a QUALIFIED issue identity. The issue number alone is not globally
unique: identity is (instance, repository, number)."
  (make-instance 'issue-ref
                 :kind kind
                 :instance instance
                 :repository repository
                 :number number))

(defmethod issue-ref-idempotency-key ((ref issue-ref))
  "Stable idempotency key: forgejo:<instance>:<repository-id>:<issue-number>."
  (format nil "~a:~a:~a:~a"
          (string-downcase (symbol-name (issue-ref-kind ref)))
          (issue-ref-instance ref)
          (issue-ref-repository ref)
          (issue-ref-number ref)))

;;; -------------------------------------------------------------- repository

(defclass research-repository ()
  ((id :initarg :id :reader repository-id :type string)
   (path :initarg :path :reader repository-path)
   (artifact-store :initarg :artifact-store :reader repository-artifact-store)
   (event-store :initarg :event-store :reader repository-event-store)
   (forgejo :initarg :forgejo :initform nil :reader repository-forgejo)))

;;; ---------------------------------------------------------------- project

(defclass project ()
  ((id :initarg :id :reader project-id :type string)
   (display-name :initarg :display-name :initform nil :reader project-display-name)
   (repository :initarg :repository :reader project-repository)
   (research-repository :initarg :research-repository :reader project-research-repository)
   (roam-slug :initarg :roam-slug :reader project-roam-slug)
   (method :initarg :method :initform :aradr :reader project-method)
   (budget :initarg :budget :reader project-budget)
   (rlm-profile :initarg :rlm-profile :initform :research :reader project-rlm-profile)
   (rlm-mode :initarg :rlm-mode :initform :direct :reader project-rlm-mode)
   (incoming :initarg :incoming :initform nil :reader project-incoming-rules)))

(defun make-project (&key id display-name repository research-repository roam-slug
                       (method :aradr) budget (rlm-profile :research)
                       (rlm-mode :direct) incoming)
  (unless (and id (plusp (length id)))
    (error 'validation-error :message "project requires a non-empty id"))
  (make-instance 'project
                 :id id
                 :display-name (or display-name id)
                 :repository repository
                 :research-repository research-repository
                 :roam-slug (or roam-slug id)
                 :method method
                 :budget budget
                 :rlm-profile rlm-profile
                 :rlm-mode rlm-mode
                 :incoming incoming))

;;; --------------------------------------------------------- request / run

(defclass research-request ()
  ((id :initarg :id :reader request-id :type string)
   (project :initarg :project :reader request-project)
   (question :initarg :question :reader request-question :type string)
   (source :initarg :source :reader request-source)                ; :api | :forgejo | :cli
   (input-kind :initarg :input-kind :initform nil :reader request-input-kind) ; :file | :text
   (input-path :initarg :input-path :initform nil :reader request-input-path)
   (issue-ref :initarg :issue-ref :initform nil :reader request-issue-ref)
   (status :initarg :status :accessor request-status)
   (run-ids :initarg :run-ids :accessor request-run-ids :type list)
   (created-at :initarg :created-at :reader request-created-at)))

(defclass research-run ()
  ((id :initarg :id :reader run-id :type string)
   (request-id :initarg :request-id :reader run-request-id :type string)
   (project :initarg :project :reader run-project)
   (status :initarg :status :accessor run-status)
   (budget :initarg :budget :reader run-budget)
   (created-at :initarg :created-at :reader run-created-at)))
