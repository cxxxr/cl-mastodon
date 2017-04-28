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
           :<status>))
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
