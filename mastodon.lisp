(defpackage :mastodon
  (:use #:cl
        #:mastodon.config
        #:mastodon.entity
        #:mastodon.base)
  (:export #:get-account
           #:get-current-user
           #:followers
           #:following
           #:account-statuses
           #:follow-account
           #:unfollow-account
           #:block-account
           #:unblock-account
           #:mute-account
           #:unmute-account
           #:relationships
           #:search-accounts

           #:search-content
           #:post-status
           ))
(in-package :mastodon)

(defmacro options (&rest options)
  (let ((glist (gensym)))
    `(let ((,glist '()))
       ,@(mapcar (lambda (o)
                   `(when ,o
                      (push (cons ,(ppcre:regex-replace-all "-" (string o) "_") ',o)
                            ,glist)))
                 (reverse options))
       ,glist)))

(defun get-account (app id)
  (let ((json (http-get app (format nil "/api/v1/accounts/~D" id))))
    (parse '<account> json)))

(defun get-current-user (app)
  (let ((json (http-get app "/api/v1/accounts/verify_credentials")))
    (parse '<account> json)))

(defun followers (app id &key max-id since-id limit)
  (let ((json (http-get app (format nil "/api/v1/accounts/~D/followers" id)
                        (options max-id since-id limit))))
    (parse-list '<account> json)))

(defun following (app id &key max-id since-id limit)
  (let ((json (http-get app (format nil "/api/v1/accounts/~D/following" id)
                        (options max-id since-id limit))))
    (parse-list '<account> json)))

(defun account-statuses (app id &key only-media exculude-replies max-id since-id limit)
  (let ((json (http-get app (format nil "/api/v1/accounts/~D/statuses" id)
                        (options only-media exculude-replies max-id since-id limit))))
    (parse-list '<status> json)))

(defun follow-account (app id)
  (let ((json (http-post app (format nil "/api/v1/accounts/~D/follow" id))))
    (parse '<relationship> json)))

(defun unfollow-account (app id)
  (let ((json (http-post app (format nil "/api/v1/accounts/~D/unfollow" id))))
    (parse '<relationship> json)))

(defun block-account (app id)
  (let ((json (http-post app (format nil "/api/v1/accounts/~D/block" id))))
    (parse '<relationship> json)))

(defun unblock-account (app id)
  (let ((json (http-post app (format nil "/api/v1/accounts/~D/unblock" id))))
    (parse '<relationship> json)))

(defun mute-account (app id)
  (let ((json (http-post app (format nil "/api/v1/accounts/~D/mute" id))))
    (parse '<relationship> json)))

(defun unmute-account (app id)
  (let ((json (http-post app (format nil "/api/v1/accounts/~D/unmute" id))))
    (parse '<relationship> json)))

(defun relationships (app &rest ids)
  (let ((json
         (http-get app "/api/v1/accounts/relationships"
                   (mapcar (lambda (id)
                             `("id[]" . ,(prin1-to-string id)))
                           ids))))
    (parse-list '<relationship> json)))

(defun search-accounts (app query &key limit)
  (let ((json (http-get app
                        "/api/v1/accounts/search"
                        (acons "q" query (options limit)))))
    (parse-list '<account> json)))

(defun blocks (app &key max-id since-id limit)
  (let ((json (http-get app
                        "/api/v1/blocks"
                        (options max-id since-id limit))))
    (parse-list '<account> json)))

(defun favourites (app &key max-id since-id limit)
  (let ((json (http-get app
                        "/api/v1/favourites"
                        (options max-id since-id limit))))
    (parse-list '<status> json)))

(defun follow-requests (app &key max-id since-id limit)
  (let ((json (http-get app
                        "/api/v1/follow_requests"
                        (options max-id since-id limit))))
    (parse-list '<account> json)))


(defun search-content (app query resolve)
  (let ((json (http-get app (format nil "/api/v1/search?q=~A&resolve=~A" query resolve))))
    (parse '<results> json)))

(defun post-status (app text &key in-reply-to-id media-ids sensitive spoiler-text visibility)
  (let ((json
         (http-post app
                    "/api/v1/statuses"
                    (acons "status" text
                           (options in-reply-to-id
                                    media-ids
                                    sensitive
                                    spoiler-text
                                    visibility)))))
    (parse '<status> json)))


#|

* GET /api/v1/accounts/:id
* GET /api/v1/accounts/verify_credentials
- PATCH /api/v1/accounts/update_credentials
* GET /api/v1/accounts/:id/followers
* GET /api/v1/accounts/:id/following
* GET /api/v1/accounts/:id/statuses
* POST /api/v1/accounts/:id/follow
* POST /api/v1/accounts/:id/unfollow
* POST /api/v1/accounts/:id/block
* POST /api/v1/accounts/:id/unblock
* POST /api/v1/accounts/:id/mute
* POST /api/v1/accounts/:id/unmute
* GET /api/v1/accounts/relationships
* GET /api/v1/accounts/search
* POST /api/v1/apps
* GET /api/v1/blocks
* GET /api/v1/favourites
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
* GET /api/v1/search
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
