(defpackage :mastodon
  (:use #:cl
        #:mastodon.config
        #:mastodon.entity
        #:mastodon.base))
(in-package :mastodon)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun option-variable (opt)
    (etypecase opt
      (symbol opt)
      (string opt)
      (cons (first opt))))
  
  (defun option-name (opt)
    (etypecase opt
      (symbol (ppcre:regex-replace-all "-" (string opt) "_"))
      (string opt)
      (cons (second opt))))
  
  (defun array-option-p (opt)
    (and (consp opt) (eq :array (third opt))))
  
  (defmacro options (&rest options)
    (let ((glist (gensym)))
      `(let ((,glist '()))
         ,@(mapcar (lambda (o)
                     `(when ,(option-variable o)
                        (push (cons ,(option-name o) ',(option-variable o))
                              ,glist)))
                   (reverse options))
         ,glist)))
  
  (defun gen-options (options)
    (cond ((null options) '())
          ((eq '&key (car options))
           `(options ,@(cdr options)))
          ((array-option-p (car options))
           `(nconc (mapcar (lambda (x)
                             (cons ,(format nil "~A[]" (option-name (car options)))
                                   (prin1-to-string x)))
                           ,(option-variable (car options)))
                   ,(gen-options (cdr options))))
          (t
           `(acons ,(option-name (car options))
                   ,(option-variable (car options))
                   ,(gen-options (cdr options))))))
  (defun options-to-lambda-list (options)
    (mapcar #'option-variable options)))

(defmacro define-api (name http-method api (&rest vars) options result-type)
  (let ((_json (gensym)))
    `(export
      (defun ,name (app ,@vars ,@(options-to-lambda-list options))
        (let ((,_json
               ,(let ((uri `(format nil ,api ,@vars)))
                  (ecase http-method
                    (:get `(http-get app ,uri ,(gen-options options)))
                    (:post `(http-post app ,uri ,(gen-options options)))
                    (:delete `(http-delete app ,uri))))))
          (declare (ignorable ,_json))
          ,(cond ((null result-type) (values))
                 ((and (consp result-type)
                       (eq 'list (car result-type)))
                  `(parse-list ',(cadr result-type) ,_json))
                 (t
                  `(parse ',result-type ,_json))))))))

(define-api get-account :get "/api/v1/accounts/~D" (id) () <account>)
(define-api get-current-user :get "/api/v1/accounts/verify_credentials" () () <account>)
(define-api followers :get "/api/v1/accounts/~D/followers" (id) (&key max-id since-id limit)
  (list <account>))
(define-api following :get "/api/v1/accounts/~D/following" (id) (&key max-id since-id limit)
  (list <account>))
(define-api account-statuses :get "/api/v1/accounts/~D/statuses" (id)
  (&key only-media exculude-replies max-id since-id limit)
  (list <status>))
(define-api follow-account :post "/api/v1/accounts/~D/follow" (id) () <relationship>)
(define-api unfollow-account :post "/api/v1/accounts/~D/unfollow" (id) () <relationship>)
(define-api block-account :post "/api/v1/accounts/~D/block" (id) () <relationship>)
(define-api unblock-account :post "/api/v1/accounts/~D/unblock" (id) () <relationship>)
(define-api mute-account :post "/api/v1/accounts/~D/mute" (id) () <relationship>)
(define-api unmute-account :post "/api/v1/accounts/~D/unmute" (id) () <relationship>)
(define-api relationships :get "/api/v1/accounts/relationships" () ((ids "id" :array))
  (list <relationship>))
(define-api search-accounts :get "/api/v1/accounts/search" () ((query "q") &key limit)
  (list <account>))
(define-api blocks :get "/api/v1/blocks" () (&key max-id since-id limit) (list <account>))
(define-api favourites :get "/api/v1/favourites" () (&key max-id since-id limit) (list <status>))
(define-api follow-requests :get "/api/v1/follow_requests" () (&key max-id since-id limit)
  (list <account>))
(define-api follow-request-authorize :post "/api/v1/follow_requests/~D/authorize" (id) () nil)
(define-api follow-request-reject :post "/api/v1/follow_requests/~D/reject" (id) () nil)
(define-api follows :post "/api/v1/follows" () (uri) <account>)
(define-api instance :get "/api/v1/instance" () () <instance>)
(define-api upload-media :post "/api/v1/media" () (file) <attachment>)
(define-api mutes :get "/api/v1/mutes" () (&key max-id since-id limit) <account>)
(define-api notifications :get "/api/v1/notifications" () (&key max-id since-id limit)
  (list <notification>))
(define-api notification :get "/api/v1/notification/~D" (id) () <notification>)
(define-api clear-notifications :post "/api/v1/notifications/clear" () () nil)
(define-api get-reports :get "/api/v1/reports" () () (list <report>))
(define-api report :post "/api/v1/reports" () (account-id (status-ids "status-ids" :array) comment)
  <report>)
(define-api search-content :get "/api/v1/search" () ((query "q") resolve) <results>)
(define-api get-status :get "/api/v1/statuses/~D" (id) () <status>)
(define-api get-statsu-context :get "/api/v1/statuses/~D/context" (id) () <context>)
(define-api get-status-card :get "/api/v1/statuses/~D/card" (id) () <card>)
(define-api get-reblogged-by :get "/api/v1/statuses/~D/reblogged_by" (id)
  (&key max-id since-id limit)
  (list <account>))
(define-api get-favourited-by :get "/api/v1/statuses/~D/favourited_by" (id)
  (&key max-id since-id limit)
  (list <account>))
(define-api post-status :post "/api/v1/statuses" ()
  (text &key in-reply-to-id media-ids sensitive spoiler-text visibility)
  <status>)
(define-api delete-status :delete "/api/v1/statuses/~D" (id) () nil)
(define-api reblog :post "/api/v1/statuses/~D/reblog" (id) () <status>)
(define-api unreblog :post "/api/v1/statuses/~D/unreblog" (id) () <status>)
(define-api favourite :post "/api/v1/statuses/~D/favourite" (id) () <status>)
(define-api unfavourite :post "/api/v1/statuses/~D/unfavourite" (id) () <status>)
(define-api get-timeline-home :get "/api/v1/timeline/home" () (&key local max-id since-id limit)
  (list <status>))
(define-api get-timeline-public :get "/api/v1/timeline/public" () (&key local max-id since-id limit)
  (list <status>))
(define-api get-timeline-hashtag :get "/api/v1/timeline/tag/~D" (hashtag)
  (&key local max-id since-id limit)
  (list <status>))


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
* GET /api/v1/follow_requests
* POST /api/v1/follow_requests/:id/authorize
* POST /api/v1/follow_requests/:id/reject
* POST /api/v1/follows
* GET /api/v1/instance
* POST /api/v1/media
* GET /api/v1/mutes
* GET /api/v1/notifications
* GET /api/v1/notifications/:id
* POST /api/v1/notifications/clear
* GET /api/v1/reports
* POST /api/v1/reports
* GET /api/v1/search
* GET /api/v1/statuses/:id
* GET /api/v1/statuses/:id/context
* GET /api/v1/statuses/:id/card
* GET /api/v1/statuses/:id/reblogged_by
* GET /api/v1/statuses/:id/favourited_by
* POST /api/v1/statuses
* DELETE /api/v1/statuses/:id
* POST /api/v1/statuses/:id/reblog
* POST /api/v1/statuses/:id/unreblog
* POST /api/v1/statuses/:id/favourite
* POST /api/v1/statuses/:id/unfavourite
* GET /api/v1/timelines/home
* GET /api/v1/timelines/public
* GET /api/v1/timelines/tag/:hashtag

|#
