
;; short hand
(defmacro e (&rest args)
  `(ext:run-shell-command ,@args))

(defun read-stream (s)
  (loop for line = (read-line s nil)
        while line
        collect line))

(defun sh (command)
  (with-open-stream (s (e command :output :stream))
    (read-stream s)))

(defun compile-prog ()
  (e "./compile.sh"))

(defun reb ()
  (load "benchmarks.lisp"))


