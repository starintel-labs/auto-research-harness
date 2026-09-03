;;;; json.lisp --- small JSON helpers over com.inuoe.jzon (v1.1.x).
;;;;
;;;; Parsed values are normalized to CL-native shapes:
;;;;   object -> hash table (string keys, EQUAL test)
;;;;   array  -> list
;;;;   string -> string, number -> number, true -> t, false -> nil,
;;;;   null   -> :null keyword
;;;;
;;;; Stringify accepts hash tables (string keys), lists, strings, numbers,
;;;; booleans and :null. Keywords must not be used as values; convert first.

(in-package #:rlm/internal)

(defun json-parse (text)
  "Parse TEXT into normalized CL values."
  (normalize (jz:parse text)))

(defun normalize (value)
  (typecase value
    (hash-table
     (let ((table (make-hash-table :test #'equal)))
       (loop :for key :being :the :hash-keys :of value
               :using (hash-value val)
             :do (setf (gethash key table) (normalize val)))
       table))
    (string value)
    (vector (map 'list #'normalize value))
    ((eql 'null) :null)
    (t value)))

(defun json-stringify (value)
  "Serialize VALUE to a JSON string."
  (jz:stringify (denormalize value)))

(defun denormalize (value)
  (typecase value
    (hash-table value)
    (cons (mapcar #'denormalize value))
    ((eql :null) 'null)
    (t value)))

(defun jref (object key)
  "Raw lookup of KEY in OBJECT (hash table)."
  (and (hash-table-p object) (gethash key object)))

(defun jget (object key &optional (default nil))
  (let ((value (jref object key)))
    (if (null value) default value)))

(defun jstr (object key)
  "Value of KEY in OBJECT coerced to string, or NIL."
  (let ((value (jref object key)))
    (when value (princ-to-string value))))

(defun jnum (object key)
  (let ((value (jref object key)))
    (when (numberp value) value)))

(defun jmap (alist)
  "Build a hash table (string keys) from ALIST of (key . value)."
  (let ((table (make-hash-table :test #'equal)))
    (loop :for (key . value) :in alist
          :do (setf (gethash (if (keywordp key)
                                 (keyword->string key)
                                 (string key))
                             table)
                    value))
    table))

(defun keyword->string (keyword)
  (string-downcase (symbol-name keyword)))

(defun now-iso8601 ()
  (local-time:format-rfc3339-timestring nil (local-time:now)))

(defun make-uuid ()
  (subseq (ironclad:byte-array-to-hex-string
           (ironclad:digest-sequence :sha256 (ironclad:make-random-salt 16)))
          0 24))
