(defpackage :mastodon
  (:use :cl
        :mastodon.config
        :mastodon.entity))
(in-package :mastodon)

(defun api-post-status (app text &key in-reply-to-id media-ids sensitive spoiler-text visibility)
  (let ((plist
         (http-post app
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
    (parse '<status> plist)))

(defun api-follow (app id)
  (let ((plist (http-post app (format nil "/api/v1/accounts/~A/follow" id))))
    (parse '<relationship> plist)))

(defun api-unfollow (app id)
  (let ((plist (http-post app (format nil "/api/v1/accounts/~A/unfollow" id))))
    (parse '<relationship> plist)))

(defun api-search (app query resolve)
  (let ((plist (http-get app (format nil "/api/v1/search?q=~A&resolve=~A" query resolve))))
    (parse '<results> plist)))


#|

* GET /api/v1/accounts/:id
* GET /api/v1/accounts/verify_credentials
- PATCH /api/v1/accounts/update_credentials
- GET /api/v1/accounts/:id/followers
- GET /api/v1/accounts/:id/following
- GET /api/v1/accounts/:id/statuses
* POST /api/v1/accounts/:id/follow
* POST /api/v1/accounts/:id/unfollow
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
