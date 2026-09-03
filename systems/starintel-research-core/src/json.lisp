;;;; json.lisp --- shared JSON helpers for the research core.

(defpackage #:research-core/json
  (:use #:cl)
  (:nicknames #:rc.j)
  (:local-nicknames (:jz :com.inuoe.jzon))
  (:export #:parse-value
           #:stringify-value
           #:to-table))

(in-package #:research-core/json)

(defun parse-value (text)
  "Parse TEXT; objects -> hash tables (string keys, EQUAL), arrays -> lists,
null -> :null."
  (norm (jz:parse text)))

(defun norm (value)
  (typecase value
    (hash-table (to-table value))
    (string value)
    (vector (map 'list #'norm value))
    ((eql 'null) :null)
    (t value)))

(defun stringify-value (pairs)
  "Serialize a flat key/value PLIST (string keys) as a JSON object."
  (jz:stringify (pairs->table pairs)))

(defun pairs->table (pairs)
  (let ((table (make-hash-table :test #'equal)))
    (loop :for (key value) :on pairs :by #'cddr
          :do (when value
                (setf (gethash (if (keywordp key)
                                   (string-downcase (symbol-name key))
                                   (string key))
                               table)
                      value)))
    table))

(defun to-table (table)
  "Copy a jzon hash table into an EQUAL hash table with normalized values."
  (let ((out (make-hash-table :test #'equal)))
    (loop :for key :being :the :hash-keys :of table
            :using (hash-value value)
          :do (setf (gethash key out) (norm value)))
    out))
