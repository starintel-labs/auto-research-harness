;;;; org.lisp --- Org keyword parsing and serialization.
;;;;
;;;; Canonical serialization belongs to CL, never to the model. The
;;;; keyword semantics mirror scripts/_roamlib.py of
;;;; lost-rob0t/starintel-auto-research: keywords are parsed case
;;;; insensitively from the front matter (after any :PROPERTIES: drawer),
;;;; first occurrence wins for reads, and upserts replace the first
;;;; match or insert after the last keyword.

(in-package #:starintel-auto-research/internal)

(defun now-iso8601 ()
  (local-time:format-rfc3339-timestring nil (local-time:now)))

(defun short-id (prefix)
  (format nil "~a-~a" prefix
          (subseq (ironclad:byte-array-to-hex-string
                   (ironclad:digest-sequence
                    :sha256
                    (babel:string-to-octets
                     (format nil "~a-~a-~a"
                             prefix
                             (get-universal-time)
                             (random most-positive-fixnum))
                     :encoding :utf-8)))
                  0 20)))

(defun dict (&rest pairs)
  (let ((table (make-hash-table :test #'equal)))
    (loop :for (key value) :on pairs :by #'cddr
          :do (setf (gethash (if (keywordp key)
                                 (string-downcase (symbol-name key))
                                 (string key))
                             table)
                    value))
    table))

(defun slugify (value)
  "Port of scripts/save-research.py slugify: lowercase, non-alnum to dashes."
  (let ((slug (string-trim "-"
                           (ppcre:regex-replace-all "[^a-z0-9]+"
                                                    (string-downcase value)
                                                    "-"))))
    (if (zerop (length slug))
        (error 'research-core:validation-error :message "invalid slug")
        slug)))

(in-package #:starintel-auto-research)

(defparameter +keyword-re+
  (ppcre:create-scanner "^[ \\t]*#\\+([A-Za-z0-9_-]+):[ \\t]*(.*?)\\s*$"))

(defun parse-org-keywords (text)
  "Return a hash table (lowercased keyword -> value, first occurrence wins)
for the front-matter keywords of TEXT. Parsing stops at the first heading."
  (let ((table (make-hash-table :test #'equal)))
    (with-input-from-string (in text)
      (loop
        :for line := (read-line in nil nil)
        :while line
        :do (cond
              ((ppcre:scan "^\\s*#\\+[A-Za-z0-9_-]+:" line)
               (multiple-value-bind (match parts)
                   (ppcre:scan-to-strings +keyword-re+ line)
                 (when match
                   (let ((key (string-downcase (aref parts 0))))
                     (unless (gethash key table)
                       (setf (gethash key table) (aref parts 1)))))))
              ((ppcre:scan "^\\*" line)
               (return))
              (t nil))))
    table))

(defun upsert-org-keyword (text key value)
  "Port of _roamlib.upsert_header: replace the first `#+key:' line with
`#+key: value', or insert after the last keyword line at the top."
  (let ((line (format nil "#+~a: ~a" (string-downcase key) value))
        (scanner (ppcre:create-scanner
                  (format nil "^[ \\t]*#\\+~a:[ \\t]*.*$"
                          (ppcre:quote-meta-chars (string-downcase key)))
                  :multi-line-mode t)))
    (multiple-value-bind (start end)
        (ppcre:scan scanner text)
      (if start
          (concatenate 'string
                       (subseq text 0 start)
                       line
                       (subseq text end))
          (with-input-from-string (in text)
            ;; insert after the last leading keyword/blank line
            (let* ((lines (loop :for l := (read-line in nil nil)
                                :while l
                                :collect l))
                   (insert-at 0))
              (loop :while (and (< insert-at (length lines))
                                (or (ppcre:scan "^\\s*#\\+" (nth insert-at lines))
                                    (zerop (length (string-trim " " (nth insert-at lines))))))
                    :do (incf insert-at))
              (let* ((result (append (subseq lines 0 insert-at)
                                     (list line)
                                     (subseq lines insert-at)))
                     (text2 (with-output-to-string (out)
                              (dolist (l result)
                                (write-line l out)))))
                (if (char= (char text (1- (length text))) #\newline)
                    text2
                    (string-right-trim (string #\newline) text2)))))))))

(defun org-section-bounds (text title)
  "Return (values start end) of the top-level section titled TITLE, or
(values nil nil). Port of validate-docs.py section_bounds."
  (let ((heading (ppcre:create-scanner
                  (format nil "^\\*\\s+~a\\s*$"
                          (ppcre:quote-meta-chars title))
                  :multi-line-mode t
                  :case-insensitive-mode t)))
    (multiple-value-bind (start end)
        (ppcre:scan heading text)
      (when start
        (let ((next (ppcre:scan "^\\*\\s+" text :start end)))
          (values start (or next (length text))))))))

(defun org-table-rows (section)
  "Parse the table rows of SECTION into lists of cell strings, skipping
separator rows. Returns (values header rows)."
  (let ((rows))
    (with-input-from-string (in section)
      (loop
        :for line := (read-line in nil nil)
        :while line
        :when (ppcre:scan "^\\s*\\|.*\\|\\s*$" line)
          :do (let* ((inner (subseq line
                                    (1+ (position #\| line))
                                    (position #\| line :from-end t)))
                     (cells (mapcar (lambda (c) (string-trim " " c))
                                    (ppcre:split "\\|" inner)))
                     (nonempty (remove-if #'zerop (mapcar #'length cells))))
                (unless (and nonempty
                             (every (lambda (c) (every (lambda (ch) (find ch "-+:")) c))
                                    nonempty))
                  (push cells rows)))))
    (setf rows (nreverse rows))
    (if rows
        (values (car rows) (cdr rows))
        (values nil nil))))
