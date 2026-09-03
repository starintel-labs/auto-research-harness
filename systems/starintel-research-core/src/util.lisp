;;;; util.lisp --- shared helpers.

(in-package #:research-core/internal)

(defun now-iso8601 ()
  (local-time:format-rfc3339-timestring nil (local-time:now)))

(defun sha256-hex (string)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha256 (babel:string-to-octets string :encoding :utf-8))))

(defun short-id (prefix)
  "Generate a stable-ish opaque identifier: PREFIX-<hex>."
  (format nil "~a-~a" prefix
          (subseq (sha256-hex (format nil "~a-~a-~a"
                                      prefix
                                      (get-universal-time)
                                      (random most-positive-fixnum)))
                  0 20)))

(defun dict (&rest pairs)
  "Build a hash table (string keys, EQUAL) from alternating key/value pairs."
  (let ((table (make-hash-table :test #'equal)))
    (loop :for (key value) :on pairs :by #'cddr
          :do (setf (gethash (if (keywordp key)
                                 (string-downcase (symbol-name key))
                                 (string key))
                             table)
                    value))
    table))
