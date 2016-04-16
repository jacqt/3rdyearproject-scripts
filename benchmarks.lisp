
;; short hand
(defmacro e (&rest args)
  `(ext:run-shell-command ,@args))

(defun compile-prog ()
  (e "./compile.sh"))


