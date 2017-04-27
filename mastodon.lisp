(defpackage :mastodon
  (:use :cl))
(in-package :mastodon)

(defvar *client*)

(defstruct client
  access-token
  created-at)

(defun register-client ()
  (let* ((result
          (dex:post "https://mstdn.jp/api/v1/apps"
                    :content '(("client_name" . "cl-mastodon")
                               ("redirect_uris" . "urn:ietf:wg:oauth:2.0:oob")
                               ("scopes" . "read write follow"))))
         (json (jojo:parse result)))
    json))

(defun get-authorization-uri (client-id)
  (format nil "https://mstdn.jp/oauth/authorize?~{~A=~A~^&~}"
          `("client_id" ,client-id
            "response_type" "code"
            "redirect_uri" "urn:ietf:wg:oauth:2.0:oob"
            "scope" "read+write+follow")))

(defun get-access-token (client-id client-secret code)
  (let ((json (jojo:parse (dex:post "https://mstdn.jp/oauth/token"
                                    :content `(("grant_type" . "authorization_code")
                                               ("redirect_uri" . "urn:ietf:wg:oauth:2.0:oob")
                                               ("client_id" . ,client-id)
                                               ("client_secret" . ,client-secret)
                                               ("code" . ,code))))))
    (setq *client*
          (make-client :access-token  (getf json :|access_token|)
                       :created-at (getf json :|created_at|)))))


(defun timelines/home (client)
  (jojo:parse
   (dex:get "https://mstdn.jp/api/v1/timelines/home"
            :headers `(("Authorization" .
                        ,(format nil "Bearer ~A" (client-access-token client)))))))

(defun toot (client text)
  (dex:post "https://mstdn.jp/api/v1/statuses"
            :headers `(("Authorization" . ,(format nil "Bearer ~A" (client-access-token client))))
            :content `(("status" . ,text))))
