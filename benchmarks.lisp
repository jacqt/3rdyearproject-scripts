
(defmacro str (&rest args)
  `(concatenate 'string ,@args))

(defmacro e (&rest args)
  `(ext:run-shell-command ,@args))

(defun read-stream (s)
  (loop for line = (read-line s nil)
        while line
        collect line))

(defmacro sh (&rest args)
  `(let ((command (str ,@args)))
     (with-open-stream (s (e command :output :stream))
       (read-stream s))))

(defmacro time-sh (&rest args)
  (let ((start-time (get-internal-real-time)))
    `(progn
       (sh ,@args)
       (float (infix (((get-internal-real-time) - ,start-time) / internal-time-units-per-second))))))

(defconstant home-dir "/home/ug13ag2/")
(defconstant chaste-py-dir (str home-dir "Chaste/python/"))
(defconstant translators.py (str chaste-py-dir "pycml/translators.py"))

(defun .cellml (n)
  (str n ".cellml"))

(defun .cu (n)
  (str n ".cu"))

(defun .cpp (n)
  (str n ".cpp"))

(defun .out (n)
  (str n ".out"))

;; short hand

(defun compile-model! (model)
  (sh "python ../ConvertCellModel.py " (.cellml model) " -t Odeint")
  (sh "mv " (.cpp model) " " (.cu model))
  (sh "nvcc -o " (.out model) " " (.cu model) " -I /home/ug13ag2/odeint/include"))

(defun use-device! ()
  (print "Configuring translator to use device now.")
  (sh "sed -i 's/USE_DEVICE = True/USE_DEVICE = False/g " translators.py))

(defun use-host! ()
  (print "Configuring translator to use host now.")
  (sh "sed -i 's/USE_DEVICE = True/USE_DEVICE = True/g " translators.py))

(defun set-num-instances! (num)
  (print (str "Configuring translator to solve for " num " instances"))
  (sh "sed -i 's/INSTANCES = .*$/INSTANCES = " num "/g " translators.py))

(defun run-device-instances (model num-instances output-file)
  (use-device!)
  (set-num-instances! num-instances)
  (compile-model! model)
  (time-sh "./" (.out model) " > " output-file))


(defun reb ()
  (load "benchmarks.lisp"))

