(defpackage :mastodon.entity
  (:use :cl)
  (:export :<account>
           :<application>
           :<attachment>
           :<card>
           :<context>
           :<error>
           :<instance>
           :<mention>
           :<notification>
           :<relationship>
           :<report>
           :<results>
           :<status>
           :parse))
(in-package :mastodon.entity)

(defmacro define-entity (name &body attributes)
  `(defclass ,name ()
     ,(mapcar (lambda (attribute)
                `(,attribute
                  :initarg ,(intern (string-downcase attribute) :keyword)))
              attributes)))

(define-entity <account>
  id
  username
  acct
  display_name
  locked
  created_at
  followers_count
  following_count
  statuses_count
  note
  url
  avatar
  avatar_static
  header
  header_static)

(define-entity <application>
  name
  website)

(define-entity <attachment>
  id
  type
  url
  remote_url
  preview_url
  text_url)

(define-entity <card>
  url
  title
  description
  image)

(define-entity <context>
  ancestores
  descendants)

(define-entity <error>
  error)

(define-entity <instance>
  url
  title
  description
  email)

(define-entity <mention>
  url
  username
  acct
  id)

(define-entity <notification>
  id
  type
  created_at
  account
  status)

(define-entity <relationship>
  id
  following
  followed_by
  blocking
  muting
  requested)

(define-entity <report>
  id
  action_taken)

(define-entity <results>
  accounts
  statuses
  hashtags)

(define-entity <status>
  id
  uri
  url
  account
  in_reply_to_id
  in_reply_to_account_id
  reblog
  content
  created_at
  reblogs_count
  favourites_count
  reblogged
  favourited
  sensitive
  spoiler_text
  visibility
  media_attachments
  mentions
  tags
  application)

(define-entity <application>
  id
  type
  url
  remote_url
  preview_url
  text_url)

(defgeneric parse (entity plist))

(defmethod parse (entity plist)
  (apply #'make-instance entity plist))

(defmethod parse ((entity (eql '<context>)) plist)
  (make-instance entity
                 :|ancestores| (mapcar (lambda (plist)
                                         (parse '<status> plist))
                                       (getf plist :|ancestores|))
                 :|descendants| (mapcar (lambda (plist)
                                          (parse '<status> plist))
                                        (getf plist :|descendants|))))

(defmethod parse ((entity (eql '<notification>)) plist)
  (make-instance entity
                 :|id| (getf plist :|id|)
                 :|type| (getf plist :|type|)
                 :|created_at| (getf plist :|created_at|)
                 :|account| (parse '<account> (getf plist :|account|))
                 :|status| (parse '<status> (getf plist :|status|))))

(defmethod parse ((entity (eql '<results>)) plist)
  (make-instance entity
                 :|accounts| (mapcar (lambda (plist)
                                       (parse '<account> plist))
                                     (getf plist :|accounts|))
                 :|statuses| (mapcar (lambda (status)
                                       (parse '<status> status))
                                     (getf plist :|statuses|))
                 :|hashtags| (getf plist :|hashtags|)))

(defmethod parse ((entity (eql '<status>)) plist)
  (make-instance entity
                 :|id| (getf plist :|id|)
                 :|uri| (getf plist :|uri|)
                 :|url| (getf plist :|url|)
                 :|account| (parse '<account> (getf plist :|account|))
                 :|in_reply_to_id| (getf plist :|in_reply_to_id|)
                 :|in_reply_to_account_id| (getf plist :|in_reply_to_account_id|)
                 :|reblog| (parse '<status> (getf plist :|reblog|))
                 :|content| (getf plist :|content|)
                 :|created_at| (getf plist :|created_at|)
                 :|reblogs_count| (getf plist :|reblogs_count|)
                 :|favourites_count| (getf plist :|favourites_count|)
                 :|reblogged| (getf plist :|reblogged|)
                 :|favourited| (getf plist :|favourited|)
                 :|sensitive| (getf plist :|sensitive|)
                 :|spoiler_text| (getf plist :|spoiler_text|)
                 :|visibility| (getf plist :|visibility|)
                 :|media_attachments| (mapcar (lambda (plist)
                                                (parse '<attachment> plist))
                                              (getf plist :|media_attachments|))
                 :|mentions| (mapcar (lambda (plist)
                                       (parse '<mention> plist))
                                     (getf plist :|mentions|))
                 :|tags| (mapcar (lambda (plist)
                                   (parse '<tag> plist))
                                 (getf plist :|tags|))
                 :|application| (parse '<application> (getf plist :|application|))))
