;;;; approval.lisp --- canonical research approval metadata.
;;;;
;;;; The approval contract follows scripts/research_approval_migration.py
;;;; and scripts/validate-docs.py of the canonical repo:
;;;;
;;;; - schema is prolog-rlm.research-approval.v1;
;;;; - seven canonical #+approval_* fields are ALL required: partial or
;;;;   duplicate metadata is invalid;
;;;; - states are exactly PENDING, APPROVED, REJECTED;
;;;; - actor, evidence, base commit and base blob are non-empty;
;;;; - lifecycle (#+status:) and approval are independent facts:
;;;;   REVIEW does not mean APPROVED.

(in-package #:starintel-auto-research)

(defparameter +approval-schema+ "prolog-rlm.research-approval.v1")

(defparameter +approval-fields+
  '("approval_schema" "approval_state" "approval_actor" "approval_evidence"
    "approval_base_commit" "approval_base_blob" "approval_decided_at")
  "All seven canonical fields, in canonical order.")

(defparameter +approval-states+ '("PENDING" "APPROVED" "REJECTED")
  "Canonical approval states. Lifecycle never implies any of these.")

(defun approval-metadata-from-keywords (keywords)
  "Extract canonical approval values from a keyword hash table. Returns
(values values-or-nil complete-p); values is an ordered plist of the seven
fields or nil when no approval_* keyword exists at all (legacy record)."
  (let* ((present (loop :for field :in +approval-fields+
                        :collect (cons field (gethash field keywords))))
         (count (count-if #'cdr present)))
    (cond
      ((zerop count) (values nil nil))
      ((/= count (length +approval-fields+))
       (error 'research-core:validation-error
              :message "partial canonical approval metadata is invalid"))
      (t (values present t)))))

(defun canonical-approval-p (text)
  "T when TEXT carries complete canonical approval metadata."
  (approval-metadata-from-keywords (parse-org-keywords text))
  (multiple-value-bind (values present)
      (approval-metadata-from-keywords (parse-org-keywords text))
    (values (and present values))))

(defun validate-approval-metadata (values)
  "VALUES is an ordered alist of the seven fields. Fails closed on:
unsupported schema, invalid state, empty actor/evidence/base fields."
  (flet ((value (field) (cdr (assoc field values))))
    (unless (= (length values) (length +approval-fields+))
      (error 'research-core:validation-error
             :message "partial canonical approval metadata is invalid"))
    (unless (string= (value "approval_schema") +approval-schema+)
      (error 'research-core:validation-error
             :message (format nil "unsupported approval schema: ~a"
                              (value "approval_schema"))))
    (unless (member (value "approval_state") +approval-states+ :test #'string=)
      (error 'research-core:validation-error
             :message (format nil "invalid canonical approval state: ~a"
                              (value "approval_state"))))
    (dolist (field '("approval_actor" "approval_evidence"
                     "approval_base_commit" "approval_base_blob"))
      (when (zerop (length (string-trim " " (value field))))
        (error 'research-core:validation-error
               :message (format nil "canonical ~a is empty" field))))
    t))

(defun render-approval-keywords (state actor evidence base-commit base-blob
                                 &optional (decided-at "NONE"))
  "Render the seven canonical keyword lines in canonical order."
  (declare (string state actor evidence base-commit base-blob))
  (validate-approval-metadata
   (list (cons "approval_schema" +approval-schema+)
         (cons "approval_state" state)
         (cons "approval_actor" actor)
         (cons "approval_evidence" evidence)
         (cons "approval_base_commit" base-commit)
         (cons "approval_base_blob" base-blob)
         (cons "approval_decided_at" decided-at)))
  (with-output-to-string (out)
    (dolist (pair (list (cons "approval_schema" +approval-schema+)
                        (cons "approval_state" state)
                        (cons "approval_actor" actor)
                        (cons "approval_evidence" evidence)
                        (cons "approval_base_commit" base-commit)
                        (cons "approval_base_blob" base-blob)
                        (cons "approval_decided_at" decided-at)))
      (format out "#+~a: ~a~%" (car pair) (cdr pair)))))

(defun approval-state-of (text)
  "The canonical approval state of TEXT, or :unmigrated when the record
carries no approval metadata. Partial metadata is an error, never a state."
  (multiple-value-bind (values present)
      (approval-metadata-from-keywords (parse-org-keywords text))
    (if present
        (intern (string-upcase (cdr (assoc "approval_state" values))) :keyword)
        :unmigrated)))

(defun lifecycle-status-of (text)
  "The lifecycle keyword (#+status:) of TEXT. This is a DIFFERENT fact
from the approval state and is never inferred from it."
  (let ((status (gethash "status" (parse-org-keywords text))))
    (if (and status (plusp (length status)))
        (intern (string-upcase status) :keyword)
        :missing)))
