(defpackage :mastodon.globals
  (:use :cl)
  (:export :+client-name+))
(in-package :mastodon.globals)

(defparameter +client-name+ "cl-mastodon")
