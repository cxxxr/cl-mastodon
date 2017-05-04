(defpackage :mastodon.config
  (:use #:cl)
  (:export #:*use-config-directory*
           #:+client-name+
           #:ensure-config-directory
           #:get-config-file))
(in-package :mastodon.config)

(defparameter *use-config-directory* t)
(defparameter +client-name+ "cl-mastodon")
(defparameter +config-directory+ (merge-pathnames ".cl-mastodon/" (user-homedir-pathname)))

(defun ensure-config-directory ()
  (when (and (not (uiop:directory-exists-p +config-directory+))
             *use-config-directory*)
    (warn "make directory: ~S" +config-directory+)
    (ensure-directories-exist +config-directory+)))

(defun get-config-file (name)
  (merge-pathnames name +config-directory+))
