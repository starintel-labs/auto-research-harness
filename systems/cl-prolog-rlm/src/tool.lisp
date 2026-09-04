;;;; tool.lisp --- model-visible tool definition, validation, and dispatch.
;;;;
;;;; A tool is a narrow semantic capability exposed to the model. The tool
;;;; system derives the wire schema projected into the provider request,
;;;; validates arguments before the handler runs, classifies side effects,
;;;; bounds wall time and output size, and records an audit trail entry.

(in-package #:rlm)

(defclass rlm-tool ()
  ((name :initarg :name :reader tool-name :type string)
   (description :initarg :description :reader tool-description :type string)
   (parameters :initarg :parameters :reader tool-parameters)
   (result :initarg :result :initform nil :reader tool-result-schema)
   (effect :initarg :effect :initform :read :reader tool-effect
           :type (member :read :write :process :network-write))
   (side-effect :initarg :side-effect :initform :none :reader tool-side-effect)
   (time-limit :initarg :time-limit :initform 1.0d0 :reader tool-time-limit)
   (max-output-bytes :initarg :max-output-bytes :initform 4096
                     :reader tool-max-output-bytes)
   (validate :initarg :validate :initform t :reader tool-validate)
   (handler :initarg :handler :reader tool-handler :type function)))

(defun coerce-schema (schema)
  "Coerce a JSON-schema given as an alist or hash table into an EQUAL
hash-table form so validation lookups work uniformly."
  (typecase schema
    (hash-table
     (let ((out (make-hash-table :test #'equal)))
       (loop :for key :being :the :hash-keys :of schema
               :using (hash-value value)
             :do (setf (gethash key out) (coerce-schema value)))
       out))
    (list
     ;; distinguish association entries (JSON objects) from arrays:
     ;; a proper list whose first element is a cons is an object
     (cond
       ((and (consp (car schema)) (listp (rest schema)))
        (let ((out (make-hash-table :test #'equal)))
          (loop :for item :in schema
                :do (typecase item
                      (cons (let ((key (car item))
                                  (value (cdr item)))
                              (setf (gethash (if (symbolp key)
                                                 (rlm.i:keyword->string key)
                                                 key)
                                             out)
                                    (coerce-schema value))))
                      (t item)))
          out))
       ((and (consp schema) (not (listp (rest schema))))
        ;; a single key/value entry like ("type" . "object")
        (let ((out (make-hash-table :test #'equal)))
          (setf (gethash (car schema) out) (coerce-schema (cdr schema)))
          out))
       (t (mapcar #'coerce-schema schema))))
    (string schema)
    (vector (map 'list #'coerce-schema schema))
    (t schema)))

(defun make-tool (&key name description parameters (effect :read) (side-effect :none)
                    (time-limit 1.0d0) (max-output-bytes 4096)
                    (validate t) (result nil) handler)
  "Create a tool. PARAMETERS is a JSON-schema alist like
\((\"type\" . \"object\") (\"properties\" . ...) (\"required\" . ...)).
HANDLER is a function of one argument (the arguments hash table) returning
a JSON-serializable value."
  (check-type name string)
  (check-type description string)
  (check-type effect (member :read :write :process :network-write))
  (unless (functionp handler)
    (error "tool ~a: HANDLER must be a function of one argument" name))
  (make-instance 'rlm-tool
                 :name name
                 :description description
                 :parameters (coerce-schema parameters)
                 :result result
                 :effect effect
                 :side-effect side-effect
                 :time-limit time-limit
                 :max-output-bytes max-output-bytes
                 :validate validate
                 :handler handler))

(defmacro define-rlm-tool (name-and-options description parameters &body handler)
  "DEFINE-RLM-TOOL search-research \"...\" ((\"type\" . \"object\") ...)
  (lambda (args) ...)"
  (destructuring-bind (name &key (effect :read) side-effect (time-limit 1.0d0)
                                 (max-output-bytes 4096) (validate t))
      (if (consp name-and-options) name-and-options (list name-and-options))
    `(make-tool :name ,(string-downcase (symbol-name name))
                :description ,description
                :parameters ,parameters
                :effect ,effect
                :side-effect ,side-effect
                :time-limit ,time-limit
                :max-output-bytes ,max-output-bytes
                :validate ,validate
                :handler (lambda (args) ,@handler))))

;;; --------------------------------------------------------------- registry

(defclass rlm-registry ()
  ((tools :initform (make-hash-table :test #'equal) :reader registry-table)))

(defun make-registry ()
  (make-instance 'rlm-registry))

(defgeneric register-tool (registry tool)
  (:documentation "Register TOOL with REGISTRY. Errors on duplicate names."))

(defmethod register-tool ((registry rlm-registry) (tool rlm-tool))
  (let ((table (registry-table registry)))
    (when (gethash (tool-name tool) table)
      (error "tool ~a is already registered" (tool-name tool)))
    (setf (gethash (tool-name tool) table) tool))
  tool)

(defgeneric registry-tools (registry)
  (:documentation "All registered tools sorted by name."))

(defmethod registry-tools ((registry rlm-registry))
  (sort (loop :for tool :being :the :hash-values :of (registry-table registry)
              :collect tool)
        #'string< :key #'tool-name))

(defun find-tool (registry name)
  (gethash (if (symbolp name) (rlm.i:keyword->string name) (string name))
           (registry-table registry)))

(defgeneric default-registry ()
  (:documentation "The process-wide default tool registry."))

(defmethod default-registry ()
  (or *default-registry* (setf *default-registry* (make-registry))))

(defvar *default-registry* nil)

;;; ------------------------------------------------------------ validation

(define-condition tool-argument-error (error)
  ((tool :initarg :tool :reader tool-argument-error-tool)
   (reason :initarg :reason :reader tool-argument-error-reason))
  (:report (lambda (c s)
             (format s "argument validation failed for tool ~a: ~a"
                     (tool-argument-error-tool c)
                     (tool-argument-error-reason c)))))

(define-condition tool-output-error (error)
  ((tool :initarg :tool :reader tool-output-error-tool)
   (bytes :initarg :bytes :reader tool-output-error-bytes))
  (:report (lambda (c s)
             (format s "tool ~a produced an oversized result (~a bytes)"
                     (tool-output-error-tool c)
                     (tool-output-error-bytes c)))))

(defun schema-type-of (value)
  (cond
    ((stringp value) "string")
    ((integerp value) "integer")
    ((numberp value) "number")
    ((eq value t) "boolean")
    ((listp value) "array")
    ((hash-table-p value) "object")
    (t nil)))

(defun validate-args (tool args)
  "Check ARGS (hash table) against the tool's declared JSON schema.
Enforces required fields and basic types, mirroring the Prolog-side schema
validation so malformed calls never reach the handler even when the model
bypasses the remote registry."
  (let* ((schema (tool-parameters tool))
         (properties (rlm.i:jref schema "properties"))
         (required (rlm.i:jref schema "required")))
    (dolist (field required)
      (unless (rlm.i:jref args (string field))
        (error 'tool-argument-error :tool (tool-name tool)
                                    :reason (format nil "missing required field ~a" field))))
    (when properties
      (loop :for key :being :the :hash-keys :of properties
              :using (hash-value spec)
            :do (let ((value (rlm.i:jref args key)))
                  (when value
                    (let* ((expected (rlm.i:jstr spec "type"))
                           (actual (schema-type-of value)))
                      (when (and expected actual
                                 (not (or (string= expected actual)
                                          (and (string= expected "number") (numberp value))
                                          (and (string= expected "array") (listp value))))
                        (error 'tool-argument-error
                               :tool (tool-name tool)
                               :reason (format nil "argument ~a expected type ~a, got ~a"
                                               key expected actual)))))))))))

(defgeneric invoke-registry-tool (registry name arguments)
  (:documentation "Validate and execute tool NAME from REGISTRY with ARGS.
Returns (values result tool-call-record)."))

(defmethod invoke-registry-tool ((registry rlm-registry) name arguments)
  (let ((tool (or (find-tool registry name)
                  (error "unknown tool: ~a" name)))
        (started (rlm.i:now-iso8601)))
    (when (tool-validate tool)
      (validate-args tool arguments))
    (let* ((value (funcall (tool-handler tool) arguments))
           (json (rlm.i:json-stringify value))
           (bytes (length (babel:string-to-octets json))))
      (when (> bytes (tool-max-output-bytes tool))
        (error 'tool-output-error :tool (tool-name tool) :bytes bytes))
      (values value
              (list :tool (tool-name tool)
                    :effect (tool-effect tool)
                    :side-effect (tool-side-effect tool)
                    :started-at started
                    :finished-at (rlm.i:now-iso8601)
                    :output-bytes bytes
                    :status :succeeded)))))

(defgeneric tool-wire-schema (tool)
  (:documentation "OpenAI-compatible function schema projected to the model."))

(defmethod tool-wire-schema ((tool rlm-tool))
  (rlm.i:jmap
   `(("type" . "function")
     ("function" . ,(rlm.i:jmap
                     `(("name" . ,(tool-name tool))
                       ("description" . ,(tool-description tool))
                       ("parameters" . ,(or (tool-parameters tool)
                                            (rlm.i:jmap '(("type" . "object")))))))))))

(defgeneric tool-registry-spec (tool)
  (:documentation "Registry schema spec sent to the Prolog-RLM bridge."))

(defmethod tool-registry-spec ((tool rlm-tool))
  (rlm.i:jmap
   `(("name" . ,(tool-name tool))
     ("description" . ,(tool-description tool))
     ("effect" . ,(string-downcase (symbol-name (tool-effect tool))))
     ("arguments" . ,(or (tool-parameters tool) (rlm.i:jmap '(("type" . "object")))))
     ("result" . ,(or (tool-result-schema tool) (rlm.i:jmap nil)))
     ("time_limit" . ,(tool-time-limit tool))
     ("max_output_bytes" . ,(tool-max-output-bytes tool)))))
