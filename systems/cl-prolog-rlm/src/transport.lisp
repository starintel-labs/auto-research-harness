;;;; transport.lisp --- hidden transport between CL and the Prolog-RLM bridge.
;;;;
;;;; The wire protocol is line-delimited JSON over the subprocess stdio:
;;;;   CL -> Prolog: {"op":"ping"}                     -> {"op":"pong"}
;;;;   CL -> Prolog: {"op":"run",...}                  (session starts)
;;;;   Prolog -> CL: {"op":"tool_call",...}            (during run)
;;;;   CL -> Prolog: {"op":"tool_result",...}          (reply)
;;;;   Prolog -> CL: {"op":"result","envelope":{...}}  (terminal)
;;;;   Prolog -> CL: {"op":"error","message":...}      (fatal, exit 1)
;;;;
;;;; A future CFFI implementation replaces this object without touching
;;;; callers; everything above rlm:run speaks to the transport protocol only.

(in-package #:rlm)

(define-condition rlm-error (error) ()
  (:report (lambda (c s) (declare (ignore c)) (format s "rlm error"))))

(define-condition rlm-transport-error (rlm-error)
  ((message :initarg :message :reader transport-error-message))
  (:report (lambda (c s) (format s "rlm transport error: ~a"
                                 (transport-error-message c)))))

(define-condition rlm-protocol-error (rlm-transport-error)
  ()
  (:report (lambda (c s) (format s "rlm protocol error: ~a"
                                 (transport-error-message c)))))

(defclass rlm-transport () ())

(defgeneric transport-open (transport)
  (:documentation "Start a session. Returns a live handle."))

(defgeneric transport-send (handle message)
  (:documentation "Send MESSAGE (hash table) as one JSONL frame."))

(defgeneric transport-read (handle)
  (:documentation "Read one JSONL frame. Returns a hash table, or :eof."))

(defgeneric transport-close (handle &key force)
  (:documentation "Close the session; FORCE terminates hard (cancellation)."))
(defgeneric transport-alive-p (handle)
  (:documentation "T when the underlying session is still alive."))

;;; ------------------------------------------------------------- subprocess

(defun find-swipl ()
  "Locate the swipl binary that has the prolog-rlm pack on its pack path."
  (let ((env (uiop:getenv "PROLOG_RLM_SWIPL")))
    (when (and env (plusp (length env)))
      (return-from find-swipl env)))
  (let ((candidates '("prolog-rlm-swipl" "swipl")))
    (dolist (candidate candidates)
      (when (zerop (nth-value 2 (uiop:run-program
                                 (list "which" candidate)
                                 :output :string :ignore-error-status t)))
        (return-from find-swipl candidate)))
    (error 'rlm-transport-error :message "no swipl found; set PROLOG_RLM_SWIPL")))

(defun find-bridge-script ()
  "Locate rlm_bridge.pl shipped with this system."
  (or (let ((env (uiop:getenv "PROLOG_RLM_BRIDGE")))
        (when (and env (plusp (length env)) (probe-file env))
          (return-from find-bridge-script (namestring (truename env)))))
      (let ((source (asdf:system-source-directory :cl-prolog-rlm)))
        (let ((path (merge-pathnames "bridge/rlm_bridge.pl" source)))
          (when (probe-file path)
            (return-from find-bridge-script (namestring (truename path))))))
      (error 'rlm-transport-error :message "rlm_bridge.pl not found; set PROLOG_RLM_BRIDGE")))

(defclass subprocess-transport (rlm-transport)
  ((swipl :initarg :swipl :initform nil :reader subprocess-swipl)
   (bridge :initarg :bridge :initform nil :reader subprocess-bridge)))

(defvar *default-transport* nil)

(defun default-transport ()
  (or *default-transport* (setf *default-transport* (make-subprocess-transport))))

(defun make-subprocess-transport (&key swipl bridge)
  (make-instance 'subprocess-transport :swipl swipl :bridge bridge))

(defmethod transport-open ((transport subprocess-transport))
  (let* ((swipl (or (subprocess-swipl transport) (find-swipl)))
         (bridge (or (subprocess-bridge transport) (find-bridge-script)))
         (process (uiop:launch-program
                   (list swipl "-q" "-s" bridge)
                   :input :stream
                   :output :stream
                   :error-output :stream
                   :element-type '(unsigned-byte 8))))
    (make-instance 'subprocess-handle :process process)))

(defclass subprocess-handle ()
  ((process :initarg :process :reader handle-process)
   (lock :initform (bt:make-lock) :reader handle-lock)))

(defun handle-streams (handle)
  "Return (values write-to-process read-from-process read-stderr)."
  (let ((process (handle-process handle)))
    (values (uiop:process-info-input process)
            (uiop:process-info-output process)
            (uiop:process-info-error-output process))))

(defun read-jsonl-frame (stream)
  "Read one UTF-8 JSONL frame from STREAM. Returns a string or :eof.
Bytes are accumulated and decoded as one UTF-8 sequence so multibyte
characters inside a frame survive."
  (let ((octets (make-array 0 :element-type '(unsigned-byte 8)
                                :adjustable t :fill-pointer 0)))
    (loop
      :for byte := (read-byte stream nil nil)
      :do (cond
            ((null byte) (return (if (plusp (length octets)) (decode-frame octets) :eof)))
            ((= byte #x0a) (return (decode-frame octets)))
            (t (vector-push-extend byte octets))))))

(defun decode-frame (octets)
  (babel:octets-to-string octets :encoding :utf-8 :errorp nil))

(defun frame->json (frame)
  (rlm.i:json-parse frame))

(defmethod transport-read ((handle subprocess-handle))
  (multiple-value-bind (to-process from-process stderr) (handle-streams handle)
    (declare (ignore to-process stderr))
    (let ((frame (read-jsonl-frame from-process)))
      (if (eq frame :eof)
          :eof
          (frame->json frame)))))

(defmethod transport-send ((handle subprocess-handle) message)
  (multiple-value-bind (to-process out stderr) (handle-streams handle)
    (declare (ignore out stderr))
    (let ((line (concatenate 'string (rlm.i:json-stringify message) (string #\newline))))
      (bt:with-lock-held ((handle-lock handle))
        (handler-case
            (progn
              (write-sequence (babel:string-to-octets line :encoding :utf-8) to-process)
              (finish-output to-process))
          (error (condition)
            (error 'rlm-transport-error
                   :message (format nil "write to bridge failed: ~a" condition))))))))

(defmethod transport-alive-p ((handle subprocess-handle))
  (uiop:process-alive-p (handle-process handle)))

(defmethod transport-close ((handle subprocess-handle) &key force)
  (let ((process (handle-process handle)))
    ;; terminate before closing streams: process death delivers EOF on the
    ;; still-open pipe so a blocked reader wakes; closing first would leave
    ;; the blocked read stuck on a closed fd
    (when force
      (ignore-errors (uiop:terminate-process process :urgent t)))
    (multiple-value-bind (in out stderr) (handle-streams handle)
      (declare (ignore stderr))
      (ignore-errors (close in :abort force))
      (ignore-errors (close out :abort force)))
    (unless force
      (ignore-errors (uiop:terminate-process process :urgent t)))))
