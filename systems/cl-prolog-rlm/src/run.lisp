;;;; run.lisp --- run/cancel/status lifecycle over the transport.
;;;;
;;;; A run returns structured state: run ID, status, result text, tool calls,
;;;; token usage, events/errors, and timing. Runs execute in a worker thread
;;;; so cancellation is always possible; status never blocks.

(in-package #:rlm)

;;; ------------------------------------------------------------------ types

(defclass rlm-context ()
  ((text :initarg :text :reader context-text :type string)
   (bytes :initarg :bytes :reader context-bytes :type integer)))

(defun make-context (text &key (max-bytes 8192))
  "Build a bounded opaque context for a run. The context is never treated as
instructions; it is bounded by MAX-BYTES and larger inputs are truncated."
  (check-type text string)
  (let* ((octets (babel:string-to-octets text :encoding :utf-8))
         (bytes (min (length octets) max-bytes))
         (bounded (babel:octets-to-string
                   (subseq octets 0 bytes)
                   :encoding :utf-8 :errorp nil)))
    (make-instance 'rlm-context :text bounded :bytes bytes)))

(defclass rlm-budget ()
  ((max-iterations :initarg :max-iterations :initform nil)
   (max-model-calls :initarg :max-model-calls :initform nil)
   (max-tool-calls :initarg :max-tool-calls :initform nil)
   (max-context-ops :initarg :max-context-ops :initform nil)
   (max-total-tokens :initarg :max-total-tokens :initform nil)
   (max-cost-usd :initarg :max-cost-usd :initform nil)
   (max-output-bytes :initarg :max-output-bytes :initform nil)
   (time-limit :initarg :time-limit :initform nil)))

(defun make-budget (&key max-iterations max-model-calls max-tool-calls
                      max-context-ops
                      max-total-tokens max-cost-usd max-output-bytes time-limit)
  (make-instance 'rlm-budget
                 :max-iterations max-iterations
                 :max-model-calls max-model-calls
                 :max-tool-calls max-tool-calls
                 :max-context-ops max-context-ops
                 :max-total-tokens max-total-tokens
                 :max-cost-usd max-cost-usd
                 :max-output-bytes max-output-bytes
                 :time-limit time-limit))

(defun budget-spec (budget)
  (when (typep budget 'rlm-budget)
    (labels ((field (slot name)
               (let ((value (slot-value budget slot)))
                 (when value (list (cons name value))))))
      (rlm.i:jmap
       (append (field 'max-iterations "max_iterations")
               (field 'max-model-calls "max_model_calls")
               (field 'max-tool-calls "max_tool_calls")
               (field 'max-context-ops "max_context_ops")
               (field 'max-total-tokens "max_total_tokens")
               (field 'max-cost-usd "max_cost_usd")
               (field 'max-output-bytes "max_output_bytes")
               (field 'time-limit "time_limit"))))))

(defclass rlm-provider ()
  ((endpoint :initarg :endpoint :initform nil :reader provider-endpoint)
   (model :initarg :model :initform nil :reader provider-model)
   (credential-env :initarg :credential-env :initform nil :reader provider-credential-env)
   (no-credential :initarg :no-credential :initform nil :reader provider-no-credential-p)))

(defun make-provider (&key endpoint model credential-env no-credential)
  (make-instance 'rlm-provider
                 :endpoint endpoint :model model
                 :credential-env credential-env :no-credential no-credential))

(defun provider-spec (provider)
  (when (typep provider 'rlm-provider)
    (labels ((field (value name)
               (when value (list (cons name value)))))
      (rlm.i:jmap
       (append (field (provider-endpoint provider) "endpoint")
               (field (provider-model provider) "model")
               (cond
                 ((provider-no-credential-p provider)
                  (list (cons "no_credential" t)))
                 ((provider-credential-env provider)
                  (list (cons "credential_env" (provider-credential-env provider))))
                 (t nil)))))))

;;; -------------------------------------------------------------------- run

(defclass rlm-run ()
  ((id :initarg :id :reader run-id :type string)
   (query :initarg :query :reader run-query :type string)
   (context :initarg :context :reader run-context :type (or null rlm-context))
   (registry :initarg :registry :reader run-registry)
   (transport :initarg :transport :reader run-transport)
   (budget :initarg :budget :reader run-budget)
   (provider :initarg :provider :reader run-provider)
   (status :initarg :status :accessor run-status
           :type (member :pending :running :succeeded :failed :cancelled))
   (result :initarg :result :initform nil :accessor run-result)
   (usage :initarg :usage :initform nil :accessor run-usage)
   (tool-calls :initarg :tool-calls :initform nil :accessor run-tool-calls)
   (events :initarg :events :initform nil :accessor run-events)
   (error :initarg :error :initform nil :accessor run-error)
   (started-at :initarg :started-at :initform nil :accessor run-started-at)
   (finished-at :initarg :finished-at :initform nil :accessor run-finished-at)
   (thread :initarg :thread :initform nil :accessor run-thread)
   (handle :initarg :handle :initform nil :accessor run-handle)
   (lock :initform (bt:make-lock) :reader run-lock)))

(defun record-event (run type &optional detail)
  (bt:with-lock-held ((run-lock run))
    (push (list :type type :at (rlm.i:now-iso8601) :detail detail)
          (run-events run))))

(defun run-request (run)
  (rlm.i:jmap
   `(("op" . "run")
     ("run_id" . ,(run-id run))
     ("query" . ,(run-query run))
     ("context" . ,(when (run-context run) (context-text (run-context run))))
     ("context_bytes" . ,(when (run-context run) (context-bytes (run-context run))))
     ("budget" . ,(when (run-budget run) (budget-spec (run-budget run))))
     ("provider" . ,(provider-spec (run-provider run)))
     ("capabilities" . ,(capabilities-spec run))
     ("tools" . ,(loop :for tool :in (registry-tools (run-registry run))
                       :collect (tool-registry-spec tool))))))

(defun capabilities-spec (run)
  "Session capability list: plain-text context operations (search/slice;
peek selectors are unsupported for text contexts) and one tool capability
per registered tool."
  (let ((context (run-context run))
        (tools (registry-tools (run-registry run))))
    (append
     (when context '("context(slice)" "context(search)"))
     (loop :for tool :in tools
           :collect (concatenate 'string "tool(" (tool-name tool) ")")))))

;;; -------------------------------------------------------------- execution

(defun handle-tool-call (handle run message)
  "Dispatch one bridge tool_call frame to the registry and reply."
  (let* ((call-id (rlm.i:jstr message "call_id"))
         (name (rlm.i:jstr message "name"))
         (arguments (rlm.i:jref message "arguments"))
         (started (rlm.i:now-iso8601))
         (tool (find-tool (run-registry run) name))
         record
         reply)
    (handler-case
        (let ((value (funcall (tool-handler tool) arguments)))
          (setf reply (rlm.i:jmap
                       `(("op" . "tool_result")
                         ("call_id" . ,call-id)
                         ("ok" . t)
                         ("value" . ,(or value :null)))))
          (setf record (list :call-id call-id :tool name :arguments arguments
                             :effect (tool-effect tool)
                             :side-effect (tool-side-effect tool)
                             :started-at started
                             :finished-at (rlm.i:now-iso8601)
                             :status :succeeded)))
      (error (condition)
        (setf reply (rlm.i:jmap
                     `(("op" . "tool_result")
                       ("call_id" . ,call-id)
                       ("ok" . nil)
                       ("error" . ,(rlm.i:jmap
                                    `(("kind" . "handler_error")
                                      ("message" . ,(format nil "~a" condition)))))))
              record (list :call-id call-id :tool name :arguments arguments
                           :effect (when tool (tool-effect tool))
                           :side-effect (when tool (tool-side-effect tool))
                           :started-at started
                           :finished-at (rlm.i:now-iso8601)
                           :status :failed
                           :error (format nil "~a" condition)))))
    (bt:with-lock-held ((run-lock run))
      (push record (run-tool-calls run)))
    (record-event run :tool-called (list :tool name :call-id call-id))
    (transport-send handle reply)))

(defun read-terminal (handle run)
  "Read messages until a terminal frame arrives; dispatch tool calls."
  (loop
    :for message := (transport-read handle)
    :do (cond
          ((eq message :eof)
           (error 'rlm-transport-error :message "bridge terminated before a terminal frame"))
          ((string= (rlm.i:jstr message "op") "tool_call")
           (handle-tool-call handle run message))
          ((string= (rlm.i:jstr message "op") "result")
           (return (rlm.i:jref message "envelope")))
          ((string= (rlm.i:jstr message "op") "error")
           (error 'rlm-protocol-error
                  :message (or (rlm.i:jstr message "message") "unspecified bridge error")))
          (t (error 'rlm-protocol-error
                    :message (format nil "unexpected frame op ~a"
                                     (rlm.i:jstr message "op")))))))

(defun parse-usage (payload)
  (let ((usage (rlm.i:jref payload "usage")))
    (when (hash-table-p usage)
      (list :model-calls (rlm.i:jnum usage "model_calls")
            :prompt-tokens (rlm.i:jnum usage "prompt_tokens")
            :completion-tokens (rlm.i:jnum usage "completion_tokens")
            :total-tokens (rlm.i:jnum usage "total_tokens")
            :cost-usd (rlm.i:jnum usage "cost_usd")))))

(defun envelope-payload (envelope)
  (let* ((payload (rlm.i:jref envelope "payload"))
         (term (rlm.i:jstr payload "$term")))
    (values payload term)))

(defun apply-outcome (run envelope)
  "Record the terminal outcome encoded in the trace ENVELOPE on RUN."
  (multiple-value-bind (payload term)
      (envelope-payload envelope)
    (cond
      ((string= term "ok")
       (let ((result (first (rlm.i:jref payload "args"))))
         (setf (run-result run) (rlm.i:jstr result "value")
               (run-usage run) (parse-usage result)
               (run-status run) :succeeded)
         (record-event run :run-completed
                       (list :turns (rlm.i:jnum result "turns")
                             :tool-calls (rlm.i:jnum result "tool_calls")
                             :usage (parse-usage result)))))
      ((string= term "error")
        (let ((error (first (rlm.i:jref payload "args"))))
          (setf (run-error run)
                (list :kind (rlm.i:jstr error "kind")
                      :phase (rlm.i:jstr error "phase")
                      :message (rlm.i:jstr error "message")
                      :cause (rlm.i:jref error "cause")
                      :usage (parse-usage error))
                (run-usage run) (parse-usage error))
          (record-event run :run-failed (run-error run))))
      (t
       (setf (run-status run) :failed
             (run-error run) (list :kind "protocol" :message "malformed terminal envelope"))
       (record-event run :run-failed (run-error run))))))

(defun execute-run (run)
  "Synchronously execute RUN over its transport."
  (when (eq (run-status run) :cancelled)
    (setf (run-finished-at run) (rlm.i:now-iso8601))
    (return-from execute-run run))
  (setf (run-status run) :running)
  (setf (run-started-at run) (rlm.i:now-iso8601))
  (record-event run :run-started nil)
  (handler-case
      (progn
        (setf (run-handle run) (transport-open (run-transport run)))
        (transport-send (run-handle run) (rlm.i:jmap '(("op" . "ping"))))
        (let ((pong (transport-read (run-handle run))))
          (unless (and (hash-table-p pong)
                       (string= (rlm.i:jstr pong "op") "pong"))
            (error 'rlm-protocol-error
                   :message (if (and (hash-table-p pong)
                                     (string= (rlm.i:jstr pong "op") "error"))
                                (format nil "bridge failed to boot: ~a"
                                        (rlm.i:jstr pong "message"))
                                "bridge did not answer ping"))))
        (transport-send (run-handle run) (run-request run))
        (let ((envelope (read-terminal (run-handle run) run)))
          (apply-outcome run envelope)))
    ((or rlm-transport-error rlm-protocol-error rlm-error) (condition)
      (when (eq (run-status run) :running)
        (setf (run-status run) :failed
              (run-error run) (list :kind "transport"
                                    :message (format nil "~a" condition)))
        (record-event run :run-failed (run-error run))))
    (error (condition)
      (when (eq (run-status run) :running)
        (setf (run-status run) :failed
              (run-error run) (list :kind "internal"
                                    :message (format nil "~a" condition)))
        (record-event run :run-failed (run-error run)))))
  (when (run-handle run)
    (ignore-errors
     (transport-close (run-handle run) :force (eq (run-status run) :cancelled)))
    (setf (run-handle run) nil))
  (setf (run-finished-at run) (rlm.i:now-iso8601))
  (when (eq (run-status run) :running)
    (setf (run-status run) :failed)))

;;; ----------------------------------------------------------------- public

(defun run (&key query registry context budget provider
              (transport (default-transport))
              (name "rlm"))
  "Execute one bounded direct-mode run synchronously. QUERY is the task text,
CONTEXT is an optional opaque rlm-context, TOOLS is a list of rlm-tool
instances. Returns the rlm-run object with structured state."
  (let* ((registry (or registry (default-registry)))
         (run (make-instance 'rlm-run
                             :id (rlm.i:make-uuid)
                             :query query
                             :context context
                             :registry registry
                             :transport transport
                             :budget budget
                             :provider provider
                             :status :pending)))
    (execute-run run)
    run))

(defun run-async (&key query registry context budget provider
                    (transport (default-transport)))
  "Start a run in a worker thread. Returns the rlm-run immediately;
poll STATUS for progress or CANCEL to stop it."
  (let* ((registry (or registry (default-registry)))
         (run (make-instance 'rlm-run
                             :id (rlm.i:make-uuid)
                             :query query
                             :context context
                             :registry registry
                             :transport transport
                             :budget budget
                             :provider provider
                             :status :pending)))
    (setf (run-thread run)
          (bt:make-thread (lambda () (execute-run run))
                          :name (format nil "rlm-run-~a" (run-id run))))
    run))

(defun cancel (run)
  "Cancel RUN. A running run is terminated hard; its status becomes
:cancelled. The worker wakes by EOF from the terminated bridge."
  (case (run-status run)
    ((:pending :running)
     (setf (run-status run) :cancelled)
     (record-event run :run-cancelled nil)
     ;; the worker may not have opened its bridge yet: wait briefly for the
     ;; handle so the termination can unblock its read
     (loop :repeat 200
           :while (and (not (run-handle run))
                       (run-thread run)
                       (bt:thread-alive-p (run-thread run)))
           :do (sleep 0.005))
     (when (run-handle run)
       ;; terminate the subprocess first: its EOF unblocks the worker read;
       ;; closing the stream first would leave the blocked read stuck
       (ignore-errors
        (uiop:terminate-process (handle-process (run-handle run)) :urgent t))
       (ignore-errors (transport-close (run-handle run) :force t))
       (setf (run-handle run) nil))
     (when (run-thread run)
       (ignore-errors (bt:join-thread (run-thread run))))
     t)
    (t nil)))

(defun status (run)
  "Structured status of RUN without blocking."
  (bt:with-lock-held ((run-lock run))
    (list :id (run-id run)
          :status (run-status run)
          :started-at (run-started-at run)
          :finished-at (run-finished-at run)
          :result (run-result run)
          :error (run-error run)
          :tool-calls (length (run-tool-calls run))
          :events (reverse (run-events run)))))
