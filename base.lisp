(defpackage :mastodon.base
  (:use #:cl
        #:mastodon.config)
  (:export #:make-app
           #:get-authorization-uri
           #:init-access-token-with-code
           #:init-access-token-with-password
           #:http-get
           #:http-post
           #:http-delete
           #:http-patch))
(in-package :mastodon.base)

(defstruct access-token
  created-at
  scope
  type
  value)

(defclass app ()
  ((server
    :initarg :server
    :reader app-server
    :type string)
   (client-secret
    :accessor app-client-secret
    :type string)
   (client-id
    :accessor app-client-id
    :type string)
   (id
    :initarg :id
    :accessor app-id
    :type integer)
   (access-token
    :accessor app-access-token
    :type access-token)))

(defun make-app (server)
  (let ((app (make-instance 'app :server (string-right-trim "/" server))))
    (register-app app)
    app))

(defun url (app api &optional query)
  (format nil "~A/~A~A"
          (app-server app)
          api
          (if (null query)
              ""
              (concatenate 'string "?" (quri:url-encode-params query)))))

(defun register-app (app)
  (ensure-config-directory)
  (let* ((apps-file (get-config-file "apps"))
         (json
          (if (uiop:file-exists-p apps-file)
              (with-open-file (in apps-file) (read in))
              (let* ((result
                      (dex:post (url app "/api/v1/apps")
                                :content `(("client_name" . ,+client-name+)
                                           ("redirect_uris" . "urn:ietf:wg:oauth:2.0:oob")
                                           ("scopes" . "read write follow")))))
                (jojo:parse result)))))
    (setf (app-client-secret app) (getf json :|client_secret|))
    (setf (app-client-id app) (getf json :|client_id|))
    (setf (app-id app) (getf json :|id|))
    (with-open-file (out apps-file
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
      (print json out))
    app))

(defun get-authorization-uri (app)
  (format nil
          (url app "/oauth/authorize"
               `(("client_id" . ,(app-client-id app))
                 ("response_type" . "code")
                 ("redirect_uri" . "urn:ietf:wg:oauth:2.0:oob")
                 ("scope" . "read write follow")))))

(flet ((f (app content)
         (let ((plist
                (jojo:parse
                 (dex:post (url app "/oauth/token")
                           :content `(("redirect_uri" . "urn:ietf:wg:oauth:2.0:oob")
                                      ("client_id" . ,(app-client-id app))
                                      ("client_secret" . ,(app-client-secret app))
                                      ,@content)))))
           (setf (app-access-token app)
                 (make-access-token
                  :created-at (getf plist :|created_at|)
                  :scope (getf plist :|scope|)
                  :type (getf plist :|token_type|)
                  :value (getf plist :|access_token|)))
           app)))
  (defun init-access-token-with-code (app code)
    (f app
       `(("grant_type" . "authorization_code")
         ("code" . ,code))))
  (defun init-access-token-with-password (app username password)
    (f app
       `(("grant_type" . "password")
         ("username" . ,username)
         ("password" . ,password)
         ("scope" . "read write follow")))))

(defun headers (app)
  `(("Authorization" .
     ,(format nil "Bearer ~A"
              (access-token-value (app-access-token app))))))

(defun http-get (app api &optional query)
  (jojo:parse
   (dex:get (url app api query)
            :headers (headers app))))

(defun http-post (app api &optional content)
  (jojo:parse
   (dex:post (url app api)
             :headers (headers app)
             :content content)))

(defun http-delete (app api)
  (dex:delete (url app api)
              :headers (headers app)))

(defun http-patch (app api &optional content)
  (dex:request (url app api)
               :method :patch
               :headers (headers app)
               :content content))
