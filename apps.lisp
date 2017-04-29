(defpackage :mastodon.apps
  (:use :cl
        :mastodon.entity
        :mastodon.base))
(in-package :mastodon.apps)

(defun api-get-account (app id)
  (let ((plist (http-get app (format nil "/api/v1/accounts/~A" id))))
    (parse '<account> plist)))

(defun api-get-current-user (app)
  (let ((plist (http-get app (format nil "/api/v1/accounts/verify_credentials"))))
    (parse '<account> plist)))
