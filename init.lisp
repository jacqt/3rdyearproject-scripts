(ql:quickload "cl-csv") ;; for csv parsing
(ql:quickload "cl-utilities") ;; general utils

(rename-package :cl-utilities :cl-utilities :utils)

;; function to make reloading easier.
(defun reload ()
  (load "test.lisp"))
(reload)
