(defpackage :mastodon
  (:use :cl
        :mastodon.globals
        :mastodon.entity))
(in-package :mastodon)

(defstruct access-token
  created-at
  scope
  type
  value)

(defclass app ()
  ((schema
    :initarg :schema
    :initform "https"
    :reader app-schema)
   (server-name
    :initarg :server-name
    :reader app-server-name
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

(defun make-app (server-name &optional (secure t))
  (make-instance 'app
                 :server-name server-name
                 :schema (if secure "https" "http")))

(defun url (app api)
  (format nil "~A://~A~A"
          (app-schema app)
          (app-server-name app)
          api))

(defun register-app (app)
  (let* ((result
          (dex:post (url app "/api/v1/apps")
                    :content `(("client_name" . ,+client-name+)
                               ("redirect_uris" . "urn:ietf:wg:oauth:2.0:oob")
                               ("scopes" . "read write follow"))))
         (json (jojo:parse result)))
    (setf (app-client-secret app) (getf json :|client_secret|))
    (setf (app-client-id app) (getf json :|client_id|))
    (setf (app-id app) (getf json :|id|))
    app))

(defun get-authorization-uri (app)
  (format nil
          (url app (format nil "/oauth/authorize?~{~A=~A~^&~}"
                           `("client_id" ,(app-client-id app)
                             "response_type" "code"
                             "redirect_uri" "urn:ietf:wg:oauth:2.0:oob"
                             "scope" "read+write+follow")))))

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
         ("password" . ,password)))))

(defun api-get (app api)
  (jojo:parse
   (dex:get (url app api)
            :headers `(("Authorization" .
                        ,(format nil "Bearer ~A"
                                 (access-token-value (app-access-token app))))))))

(defun api-post (app api content)
  (jojo:parse
   (dex:post (url app api)
             :headers `(("Authorization" .
                         ,(format nil "Bearer ~A"
                                  (access-token-value (app-access-token app)))))
             :content content)))

(defun get-account (app)
  (let ((plist (api-get app (format nil "/api/v1/accounts/~A" (app-id app)))))
    (apply #'make-instance '<account> plist)))

(defun get-current-user (app)
  (let ((plist (api-get app (format nil "/api/v1/accounts/verify_credentials"))))
    (apply #'make-instance '<account> plist)))

(defun post-status (app text &key in-reply-to-id media-ids sensitive spoiler-text visibility)
  (let ((plist
         (api-post app
                   "/api/v1/statuses"
                   `(("status" . ,text)
                     ,@(when in-reply-to-id
                         `(("in_reply_to_id" . ,in-reply-to-id)))
                     ,@(when media-ids
                         `(("media_ids" . ,media-ids)))
                     ,@(when sensitive
                         `(("sensitive" . ,sensitive)))
                     ,@(when spoiler-text
                         `(("spoiler_text" . ,spoiler-text)))
                     ,@(when visibility
                         `(("visibility" . ,visibility)))))))
    (apply #'make-instance '<status> plist)))


#|

* GET /api/v1/accounts/:id
* GET /api/v1/accounts/verify_credentials
- PATCH /api/v1/accounts/update_credentials
- GET /api/v1/accounts/:id/followers
- GET /api/v1/accounts/:id/following
- GET /api/v1/accounts/:id/statuses
- POST /api/v1/accounts/:id/follow
- POST /api/v1/accounts/:id/unfollow
- POST /api/v1/accounts/:id/block
- POST /api/v1/accounts/:id/unblock
- POST /api/v1/accounts/:id/mute
- POST /api/v1/accounts/:id/unmute
- GET /api/v1/accounts/relationships
- GET /api/v1/accounts/search
- POST /api/v1/apps
- GET /api/v1/blocks
- GET /api/v1/favourites
- GET /api/v1/follow_requests
- POST /api/v1/follow_requests/:id/authorize
- POST /api/v1/follow_requests/:id/reject
- POST /api/v1/follows
- GET /api/v1/instance
- POST /api/v1/media
- GET /api/v1/mutes
- GET /api/v1/notifications
- GET /api/v1/notifications/:id
- POST /api/v1/notifications/clear
- GET /api/v1/reports
- POST /api/v1/reports
- GET /api/v1/search
- GET /api/v1/statuses/:id
- GET /api/v1/statuses/:id/context
- GET /api/v1/statuses/:id/card
- GET /api/v1/statuses/:id/reblogged_by
- GET /api/v1/statuses/:id/favourited_by
* POST /api/v1/statuses
- DELETE /api/v1/statuses/:id
- POST /api/v1/statuses/:id/reblog
- POST /api/v1/statuses/:id/unreblog
- POST /api/v1/statuses/:id/favourite
- POST /api/v1/statuses/:id/unfavourite
- GET /api/v1/timelines/home
- GET /api/v1/timelines/public
- GET /api/v1/timelines/tag/:hashtag

|#
