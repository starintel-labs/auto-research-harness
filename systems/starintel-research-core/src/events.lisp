;;;; events.lisp --- append-only event store and replay.
;;;;
;;;; Every state change is an append-only event with: event identifier, type,
;;;; entity id, previous/next state, timestamp, actor, run configuration hash,
;;;; payload hash, and evidence references (STAR-RESEARCH-PIPELINE-001 ledger
;;;; model). Duplicate event ids are ignored on replay; a transition with an
;;;; unexpected previous state is rejected. Domain event type vocabulary
;;;; follows spec/09-security-events.org.

(in-package #:research-core)

(defstruct (event
            (:constructor make-event-full)
            (:conc-name event-))
  id
  type
  entity-id
  previous-state
  next-state
  timestamp
  actor
  config-hash
  payload-hash
  evidence
  payload)

(defun make-event (type entity-id &key previous-state next-state
                                      (actor "system") config-hash payload
                                      (evidence nil evidence-p))
  (make-event-full
   :id (rc.i:short-id "evt")
   :type type
   :entity-id entity-id
   :previous-state previous-state
   :next-state next-state
   :timestamp (rc.i:now-iso8601)
   :actor actor
   :config-hash config-hash
   :payload-hash (when payload (rc.i:sha256-hex (format nil "~s" payload)))
   :evidence (if evidence-p evidence (and payload '("payload")))
   :payload payload))

;;; --------------------------------------------------------------- store

(defclass event-store ()
  ()
  (:documentation "Protocol for append-only event storage."))

(defgeneric append-event (store event)
  (:documentation "Durably append EVENT. Must preserve order and content."))

(defgeneric replay-events (store &key entity-id event-types from)
  (:documentation "Return events in append order, optionally filtered."))

(defclass jsonl-event-store (event-store)
  ((path :initarg :path :reader jsonl-store-path)
   (lock :initform (bt:make-lock) :reader jsonl-store-lock)))

(defun make-jsonl-event-store (path)
  (make-instance 'jsonl-event-store :path path))

(defun event->line (event)
  (rc.j:stringify-value
   (list "id" (event-id event)
         "type" (string-downcase (symbol-name (event-type event)))
         "entity_id" (event-entity-id event)
         "previous_state" (and (event-previous-state event)
                               (string-downcase
                                (symbol-name (event-previous-state event))))
         "next_state" (and (event-next-state event)
                           (string-downcase
                            (symbol-name (event-next-state event))))
         "timestamp" (event-timestamp event)
         "actor" (event-actor event)
         "config_hash" (event-config-hash event)
         "payload_hash" (event-payload-hash event)
         "evidence" (event-evidence event)
         "payload" (event-payload event))))

(defun line->event (line)
  "Parse one JSONL event line via jzon."
  (let* ((value (rc.j:parse-value line))
         (table (rc.j:to-table value)))
    (flet ((getf* (key) (gethash key table)))
      (make-event-full
       :id (getf* "id")
       :type (intern (string-upcase (or (getf* "type") "unknown")) :keyword)
       :entity-id (getf* "entity_id")
       :previous-state (when (getf* "previous_state")
                         (intern (string-upcase (getf* "previous_state")) :keyword))
       :next-state (when (getf* "next_state")
                     (intern (string-upcase (getf* "next_state")) :keyword))
       :timestamp (getf* "timestamp")
       :actor (getf* "actor")
       :config-hash (getf* "config_hash")
       :payload-hash (getf* "payload_hash")
       :evidence (getf* "evidence")
       :payload (getf* "payload")))))

(defmethod append-event :before ((store jsonl-event-store) event)
  (declare (ignore event))
  (ensure-directories-exist (jsonl-store-path store)))

(defmethod append-event ((store jsonl-event-store) event)
  (bt:with-lock-held ((jsonl-store-lock store))
    (with-open-file (out (jsonl-store-path store)
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :append
                         :external-format :utf-8)
      (write-line (event->line event) out)))
  event)

(defmethod replay-events ((store jsonl-event-store) &key entity-id event-types from)
  (when (probe-file (jsonl-store-path store))
    (let ((events))
      (with-open-file (in (jsonl-store-path store)
                          :direction :input
                          :external-format :utf-8)
        (loop :for line := (read-line in nil nil)
              :while line
              :unless (str:blankp line)
                :do (let ((event (line->event line)))
                      (when (and (or (null entity-id)
                                     (string= (event-entity-id event) entity-id))
                                 (or (null event-types)
                                     (member (event-type event) event-types))
                                 (or (null from)
                                     (string>= (event-timestamp event) from)))
                        (push event events)))))
      (nreverse events))))
