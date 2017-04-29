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

(defun get-account (app id)
  (let ((plist (http-get app (format nil "/api/v1/accounts/~D" id))))
    (parse '<account> plist)))

(defun get-current-user (app)
  (let ((plist (http-get app "/api/v1/accounts/verify_credentials")))
    (parse '<account> plist)))

(defun followers (app id &key max-id since-id limit)
  (let ((json (http-get app (format nil "/api/v1/accounts/~D/followers" id)
                        (options max-id since-id limit))))
    (mapcar (lambda (plist)
              (parse '<account> plist))
            json)))

(defun following (app id &key max-id since-id limit)
  (let ((json (http-get app (format nil "/api/v1/accounts/~D/following" id)
                        (options max-id since-id limit))))
    (mapcar (lambda (plist)
              (parse '<account> plist))
            json)))

(defun account-statuses (app id &key only-media exculude-replies max-id since-id limit)
  (let ((json (http-get app (format nil "/api/v1/accounts/~D/statuses" id)
                        (options only-media exculude-replies max-id since-id limit))))
    (mapcar (lambda (plist)
              (parse '<status> plist))
            json)))

(defun follow-account (app id)
  (let ((plist (http-post app (format nil "/api/v1/accounts/~D/follow" id))))
    (parse '<relationship> plist)))

(defun unfollow-account (app id)
  (let ((plist (http-post app (format nil "/api/v1/accounts/~D/unfollow" id))))
    (parse '<relationship> plist)))

(defun block-account (app id)
  (let ((plist (http-post app (format nil "/api/v1/accounts/~D/block" id))))
    (parse '<relationship> plist)))

(defun unblock-account (app id)
  (let ((plist (http-post app (format nil "/api/v1/accounts/~D/unblock" id))))
    (parse '<relationship> plist)))

(defun mute-account (app id)
  (let ((plist (http-post app (format nil "/api/v1/accounts/~D/mute" id))))
    (parse '<relationship> plist)))

(defun unmute-account (app id)
  (let ((plist (http-post app (format nil "/api/v1/accounts/~D/unmute" id))))
    (parse '<relationship> plist)))

(defun relationships (app &rest ids)
  (let ((json
         (http-get app "/api/v1/accounts/relationships"
                   (mapcar (lambda (id)
                             `("id[]" . ,(prin1-to-string id)))
                           ids))))
    (mapcar (lambda (plist)
              (parse '<relationship> plist))
            json)))

(defun search-accounts (app query &key limit)
  (let ((json (http-get app
                        "/api/v1/accounts/search"
                        (acons "q" query (options limit)))))
    (mapcar (lambda (plist)
              (parse '<account> plist))
            json)))
