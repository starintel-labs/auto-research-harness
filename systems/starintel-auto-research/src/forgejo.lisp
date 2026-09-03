;;;; forgejo.lisp --- Forgejo request plane adapter.
;;;;
;;;; All Forgejo calls go through this small adapter: get repository,
;;;; get/list/create/update issue, comment issue. Issue identity is
;;;; QUALIFIED (instance, repository, issue number); issue numbers alone
;;;; are not globally unique. API-origin requests create one issue and
;;;; are idempotent against the webhook round-trip.

(in-package #:starintel-auto-research)

(defclass forgejo-client ()
  ((base-url :initarg :base-url :reader forgejo-base-url)
   (token-env :initarg :token-env :initform "FORGEJO_TOKEN" :reader forgejo-token-env)
   (token :initarg :token :initform nil :reader forgejo-token)
   (http-fn :initarg :http-fn :initform nil :reader forgejo-http-fn)))

(defun make-forgejo-client (base-url &key (token-env "FORGEJO_TOKEN") token http-fn)
  "HTTP-FN overrides the HTTP layer for tests: a function of
(method url headers body) -> (values status response-body-string)."
  (make-instance 'forgejo-client
                 :base-url (string-right-trim "/" base-url)
                 :token-env token-env
                 :token token
                 :http-fn http-fn))

(defun forgejo-resolved-token (client)
  (or (forgejo-token client)
      (let ((env (uiop:getenv (forgejo-token-env client))))
        (and env (plusp (length env)) env))))

(defun forgejo-request (client method path &key (headers (make-hash-table :test #'equal))
                                                 body)
  "One Forgejo API call. Never logs the token; the credential is attached
at request time only."
  (let* ((url (format nil "~a/api/v1~a" (forgejo-base-url client) path))
         (headers (copy-hash-table-for-headers headers))
         (token (forgejo-resolved-token client)))
    (when token
      (setf (gethash "Authorization" headers) (concatenate 'string "token " token)))
    (setf (gethash "Content-Type" headers) "application/json")
    (let ((body (when body (rc.j:stringify-value body))))
      (if (forgejo-http-fn client)
          (funcall (forgejo-http-fn client) method url headers body)
          (http-request-default method url headers body)))))

(defun copy-hash-table-for-headers (headers)
  (let ((out (make-hash-table :test #'equal)))
    (when headers
      (loop :for k :being :the :hash-keys :of headers
              :using (hash-value v)
            :do (setf (gethash k out) v)))
    out))

(defun http-request-default (method url headers body)
  "Default adapter over dexador; returns (values status body-string)."
  (let ((header-alist (loop :for k :being :the :hash-keys :of headers
                              :using (hash-value v)
                            :collect (cons k v))))
    (multiple-value-bind (response status)
        (dex:request url
                     :method (intern (string-upcase method) :keyword)
                     :headers header-alist
                     :content (or body "")
                     :force-binary nil)
      (values status
              (if (stringp response) response (princ-to-string response))))))

(defun forgejo-repo-path (repository)
  "Repository identity string like 'lost-rob0t/starintel-auto-research'."
  (string repository))

(defun forgejo-get-repo (client repository)
  (forgejo-request client :get (format nil "/repos/~a" repository)))

(defun forgejo-get-issue (client repository number)
  (forgejo-request client :get (format nil "/repos/~a/issues/~a" repository number)))

(defun forgejo-list-issues (client repository &key state labels)
  (let ((query (format nil "/repos/~a/issues?state=~a" repository (or state "open"))))
    (when labels
      (setf query (format nil "~a&labels=~a" query labels)))
    (forgejo-request client :get query)))

(defun forgejo-create-issue (client repository title body &key labels)
  (forgejo-request client :post (format nil "/repos/~a/issues" repository)
                   :body (append (list "title" title "body" body)
                                 (when labels (list "labels" labels)))))

(defun forgejo-update-issue (client repository number &key title body state)
  (forgejo-request client :patch (format nil "/repos/~a/issues/~a" repository number)
                   :body (remove nil
                                 (list (when title (cons "title" title))
                                       (when body (cons "body" body))
                                       (when state (cons "state" state))))))

(defun forgejo-comment-issue (client repository number body)
  (forgejo-request client :post (format nil "/repos/~a/issues/~a/comments" repository number)
                   :body (list "body" body)))

(defun forgejo-issue-ref (instance repository number)
  "Qualified identity: the idempotency key form of research-core's issue-ref."
  (research-core:issue-ref-idempotency-key
   (research-core:make-issue-ref :forgejo instance repository number)))

(defun forgejo-webhook-verified-p (payload secret)
  "Forgejo webhook verification is separate from normal website API
authentication. v1 accepts payloads that carry the configured secret in
their signature field; the web layer computes HMAC-SHA256 over the raw body
and never trusts the parsed body alone."
  (let ((signature (gethash "secret" (if (hash-table-p payload) payload (sar.i:dict)))))
    (and secret signature (string= signature secret))))

(defun forgejo-issue-from-payload (payload)
  "Normalize a Forgejo webhook issue payload into
(values instance repository number action title body labels)."
  (let* ((instance (gethash "instance" payload))
         (repository (gethash "repository" payload))
         (issue (gethash "issue" payload)))
    (when (and instance repository issue)
      (values instance
              repository
              (gethash "number" issue)
              (gethash "action" payload)
              (gethash "title" issue)
              (gethash "body" issue)
              (gethash "labels" issue)))))
