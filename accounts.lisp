(defpackage :mastodon.accounts
  (:use :cl
        :mastodon.entity
        :mastodon.base))
(in-package :mastodon.accounts)

(defmacro options (&rest options)
  (let ((glist (gensym)))
    `(let ((,glist '()))
       ,@(mapcar (lambda (o)
                   `(when ,o
                      (push (cons ,(ppcre:regex-replace-all "-" (string o) "_") ',o)
                            ,glist)))
                 (reverse options))
       ,glist)))

(defun api-get-account (app id)
  (let ((plist (http-get app (format nil "/api/v1/accounts/~D" id))))
    (parse '<account> plist)))

(defun api-get-current-user (app)
  (let ((plist (http-get app "/api/v1/accounts/verify_credentials")))
    (parse '<account> plist)))

(defun api-followers (app id &key max-id since-id limit)
  (let ((json (http-get app (format nil "/api/v1/accounts/~D/followers" id)
                        (options max-id since-id limit))))
    (mapcar (lambda (plist)
              (parse '<account> plist))
            json)))

(defun api-following (app id &key max-id since-id limit)
  (let ((json (http-get app (format nil "/api/v1/accounts/~D/following" id)
                        (options max-id since-id limit))))
    (mapcar (lambda (plist)
              (parse '<account> plist))
            json)))

(defun api-account-statuses (app id &key only-media exculude-replies max-id since-id limit)
  (let ((json (http-get app (format nil "/api/v1/accounts/~D/statuses" id)
                        (options only-media exculude-replies max-id since-id limit))))
    (mapcar (lambda (plist)
              (parse '<status> plist))
            json)))
