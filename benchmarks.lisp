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

(defun reb ()
  (load "benchmarks.lisp"))

(defun compile-model (model)
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
