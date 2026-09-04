;;;; dsl.lisp --- trusted project and ARADR init DSL.
;;;;
;;;; Projects and workflow policy are trusted Common Lisp init
;;;; configuration, not LLM-generated data. The init file may execute
;;;; trusted CL; issue bodies, HTTP input, web content, Org artifacts, and
;;;; model output are NEVER evaluated.

(in-package #:starintel-auto-research)

(defclass budget-profile ()
  ((name :initarg :name :reader budget-profile-name)
   (wall-clock-seconds :initarg :wall-clock-seconds :initform 7200)
   (input-tokens :initarg :input-tokens :initform 1000000)
   (output-tokens :initarg :output-tokens :initform 100000)
   (max-candidates :initarg :max-candidates :initform 5)
   (max-active-branches :initarg :max-active-branches :initform 3)
   (max-depth :initarg :max-depth :initform 4)))

(defclass research-repository-config ()
  ((name :initarg :name :reader research-repository-config-name)
   (path :initarg :path :reader research-repository-config-path)
   (artifact-store :initarg :artifact-store :initform :org-roam)
   (event-store :initarg :event-store :initform :jsonl)
   (forgejo :initarg :forgejo :initform nil :reader research-repository-forgejo)))

(defclass project-config ()
  ((id :initarg :id :reader project-config-id)
   (repository :initarg :repository :reader project-config-repository)
   (research-repository :initarg :research-repository
                        :reader project-config-research-repository)
   (method :initarg :method :initform :aradr :reader project-config-method)
   (roam-project :initarg :roam-project :reader project-config-roam-slug)
   (budget :initarg :budget :reader project-config-budget-name)
   (rlm :initarg :rlm :initform '(:mode :direct :profile :research))
   (incoming :initarg :incoming :initform nil :reader project-config-incoming)))

(defmethod project-config-rlm-mode ((config project-config))
  (getf (slot-value config 'rlm) :mode :direct))

(defmethod project-config-rlm-profile ((config project-config))
  (getf (slot-value config 'rlm) :profile :research))

(defvar *budget-profiles* (make-hash-table :test #'equal))
(defvar *research-repositories* (make-hash-table :test #'equal))
(defvar *project-configs* (make-hash-table :test #'equal))

(defun budget-profiles () *budget-profiles*)
(defun research-repositories () *research-repositories*)
(defun project-configs () *project-configs*)

(defun clear-configs ()
  "Reset the trusted config registries (used between init loads/tests)."
  (setf *budget-profiles* (make-hash-table :test #'equal)
        *research-repositories* (make-hash-table :test #'equal)
        *project-configs* (make-hash-table :test #'equal)))

(defmacro define-budget (name &key wall-clock-seconds input-tokens output-tokens
                                 max-candidates max-active-branches max-depth)
  `(setf (gethash ,(string-downcase (symbol-name name)) *budget-profiles*)
         (make-instance 'budget-profile
                        :name ,(string-downcase (symbol-name name))
                        ,@(when wall-clock-seconds
                            `(:wall-clock-seconds ,wall-clock-seconds))
                        ,@(when input-tokens `(:input-tokens ,input-tokens))
                        ,@(when output-tokens `(:output-tokens ,output-tokens))
                        ,@(when max-candidates `(:max-candidates ,max-candidates))
                        ,@(when max-active-branches
                            `(:max-active-branches ,max-active-branches))
                        ,@(when max-depth `(:max-depth ,max-depth)))))

(defmacro define-research-repository (name &key path artifact-store event-store forgejo)
  `(let ((config (make-instance 'research-repository-config
                                :name ,(string-downcase (symbol-name name))
                                :path ,path
                                :artifact-store ,(or artifact-store :org-roam)
                                :event-store ,(or event-store :jsonl)
                                :forgejo ',forgejo)))
     (unless ,path
       (error 'research-core:validation-error
              :message (format nil "research repository ~a requires a path"
                               ,(string-downcase (symbol-name name)))))
     (setf (gethash ,(string-downcase (symbol-name name)) *research-repositories*)
           config)))

(defmacro define-project (id &key repository research-repository roam-project method
                                 budget rlm incoming)
  "Define a project from TRUSTED init configuration. Reference names are
symbols; forgejo/rlm/incoming are literal data and are never evaluated."
  (let ((id-string (string-downcase (symbol-name id)))
        (repo-name (and research-repository (string-downcase (symbol-name research-repository))))
        (budget-name (and budget (string-downcase (symbol-name budget)))))
    `(progn
       (unless ,repository
         (error 'research-core:validation-error
                :message (format nil "project ~a requires a repository" ,id-string)))
       (unless (and ,repo-name
                    (gethash ,repo-name *research-repositories*))
         (error 'research-core:validation-error
                :message (format nil "project ~a references unknown research repository ~a"
                                 ,id-string ,repo-name)))
       (unless (and ,budget-name
                    (gethash ,budget-name *budget-profiles*))
         (error 'research-core:validation-error
                :message (format nil "project ~a references unknown budget ~a"
                                 ,id-string ,budget-name)))
       (setf (gethash ,id-string *project-configs*)
             (make-instance 'project-config
                            :id ,id-string
                            :repository ,repository
                            :research-repository (gethash ,repo-name
                                                          *research-repositories*)
                            :roam-project ,(or roam-project id-string)
                            :method ',(or method :aradr)
                            :budget ,budget-name
                            :rlm ',rlm
                            :incoming ',incoming)))))

(defun find-budget-profile (name)
  (gethash (string-downcase (string name)) *budget-profiles*))

(defun find-research-repository (name)
  (gethash (string-downcase (string name)) *research-repositories*))

(defun find-project-config (id)
  (gethash (string-downcase (string id)) *project-configs*))
