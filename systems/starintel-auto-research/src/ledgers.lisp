;;;; ledgers.lisp --- append-only run and candidate ledgers.
;;;;
;;;; Uses the approved ledger model (STAR-RESEARCH-PIPELINE-001):
;;;; append-only JSON Lines at roam/.research-runs and
;;;; roam/.research-candidates; each line records event id, event type,
;;;; entity id, previous state, next state, timestamp, actor, run
;;;; configuration hash, payload hash, and evidence references. State is
;;;; reconstructed by replay: duplicate event ids are ignored and a
;;;; transition with an unexpected previous state is rejected.

(in-package #:starintel-auto-research)

(defclass ledger ()
  ((path :initarg :path :reader ledger-path)
   (seen :initform (make-hash-table :test #'equal) :reader ledger-seen)
   (lock :initform (bt:make-lock) :reader ledger-lock)))

(defun make-run-ledger (roam-path)
  (make-instance 'ledger :path (merge-pathnames ".research-runs" roam-path)))

(defun make-candidate-ledger (roam-path)
  (make-instance 'ledger :path (merge-pathnames ".research-candidates" roam-path)))

(defun ledger-line (event)
  (rc.j:stringify-value
   (list "event_id" (gethash "event_id" event)
         "event_type" (gethash "event_type" event)
         "entity_id" (gethash "entity_id" event)
         "previous_state" (gethash "previous_state" event)
         "next_state" (gethash "next_state" event)
         "timestamp" (gethash "timestamp" event)
         "actor" (gethash "actor" event)
         "config_hash" (gethash "config_hash" event)
         "payload_hash" (gethash "payload_hash" event)
         "evidence" (gethash "evidence" event))))

(defun ledger-record (event-type entity-id &key previous-state next-state
                                       (actor "auto-research-runtime")
                                       config-hash payload evidence)
  (let ((event (sar.i:dict
                "event_id" (sar.i:short-id "lev")
                "event_type" (string-downcase (symbol-name event-type))
                "entity_id" entity-id
                "previous_state" (and previous-state
                                      (string-downcase (symbol-name previous-state)))
                "next_state" (and next-state
                                  (string-downcase (symbol-name next-state)))
                "timestamp" (sar.i:now-iso8601)
                "actor" actor
                "config_hash" config-hash
                "payload_hash" (when payload
                                 (ironclad:byte-array-to-hex-string
                                  (ironclad:digest-sequence
                                   :sha256 (babel:string-to-octets
                                            (format nil "~s" payload)
                                            :encoding :utf-8))))
                "evidence" evidence)))
    (when payload
      (loop :for key :being :the :hash-keys :of payload
              :using (hash-value value)
            :do (setf (gethash key event) value)))
    event))

(defun ledger-append (ledger event)
  "Append one event line. The ledger is append-only; malformed writes
abort the transition (the run loop reports the failure)."
  (bt:with-lock-held ((ledger-lock ledger))
    (ensure-directories-exist (ledger-path ledger))
    (with-open-file (out (ledger-path ledger)
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :append
                         :external-format :utf-8)
      (write-line (ledger-line event) out)))
  event)

(defun ledger-replay (ledger)
  "Replay the ledger in order. Returns (values state events):
- state maps entity id -> next-state keyword;
- duplicate event ids are ignored;
- a transition whose previous state does not match the current state is
  rejected with a transition error."
  (let ((events))
    (when (probe-file (ledger-path ledger))
      (with-open-file (in (ledger-path ledger)
                          :direction :input
                          :external-format :utf-8)
        (loop
          :for line := (read-line in nil nil)
          :while line
          :unless (str:blankp line)
            :do (push (rc.j:to-table (rc.j:parse-value line)) events))))
    (setf events (nreverse events))
    (let ((seen (make-hash-table :test #'equal))
          (state (make-hash-table :test #'equal)))
      (dolist (event events)
        (let* ((id (gethash "event_id" event))
               (entity (gethash "entity_id" event))
               (previous (gethash "previous_state" event))
               (next (gethash "next_state" event)))
          (unless (and id entity)
            (error 'research-core:validation-error
                   :message (format nil "malformed ledger record: ~a" event)))
          (unless (gethash id seen)
            (setf (gethash id seen) t)
            (let* ((current (gethash entity state))
                   (previous (and previous (intern (string-upcase previous) :keyword)))
                   (next (and next (intern (string-upcase next) :keyword))))
              (when (and current previous (not (equal current previous)))
                (error 'research-core:transition-error
                       :message (format nil
                                        "transition with unexpected previous state for ~a: ledger says ~a, replay has ~a"
                                        entity previous current)))
              (when next
                (setf (gethash entity state) next))))))
      (values state events))))
