(defpackage :mastodon.accounts
  (:use :cl
        :mastodon.entity
        :mastodon.base))
(in-package :mastodon.accounts)

(defun api-get-account (app id)
  (let ((plist (http-get app (format nil "/api/v1/accounts/~A" id))))
    (parse '<account> plist)))

(defun api-get-current-user (app)
  (let ((plist (http-get app "/api/v1/accounts/verify_credentials")))
    (parse '<account> plist)))

(defun api-followers (app id &key max-id since-id limit)
  (let ((json (http-get app (format nil "/api/v1/accounts/~A/followers" id)
                        `(,@(when max-id `(("max_id" . ,max-id)))
                          ,@(when since-id `(("since_id" . ,since-id)))
                          ,@(when limit `(("limit" . ,limit)))))))
    (mapcar (lambda (plist)
              (parse '<account> plist))
            json)))

(defun api-following (app id &key max-id since-id limit)
  (let ((json (http-get app (format nil "/api/v1/accounts/~A/following" id)
                        `(,@(when max-id `(("max_id" . ,max-id)))
                          ,@(when since-id `(("since_id" . ,since-id)))
                          ,@(when limit `(("limit" . ,limit)))))))
    (mapcar (lambda (plist)
              (parse '<account> plist))
            json)))

(defun api-statuses (app id &key only-media exculude-replies max-id since-id limit)
  )
