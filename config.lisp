(defpackage :mastodon.config
  (:use :cl)
  (:export :+client-name+
           :ensure-config-directory
           :get-config-file))
(in-package :mastodon.config)

(defparameter +client-name+ "cl-mastodon")
(defparameter +config-directory+ (merge-pathnames ".cl-mastodon/" (user-homedir-pathname)))

(defun ensure-config-directory ()
  (unless (uiop:directory-exists-p +config-directory+)
    (ensure-directories-exist +config-directory+)))

(defun get-config-file (name)
  (merge-pathnames name +config-directory+))
