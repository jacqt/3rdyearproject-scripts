
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
  `(let ((start-time (get-internal-real-time)))
     (progn
       (sh ,@args)
       (float (infix (((get-internal-real-time) - start-time) / internal-time-units-per-second))))))

(defmacro average (num body)
  "Runs the body num times and returns the average"
  `(/ (foldr
        (fn (acc nv) (+ acc ,body))
        0 (range 0 ,num 1))
      ,num))

(defconstant home-dir "/home/ug13ag2/")
(defconstant chaste-py-dir (str home-dir "Chaste/python/"))
(defconstant translators.py (str chaste-py-dir "pycml/translators.py"))
(defconstant include-odeint (str " -I " home-dir "odeint/include"))
(defconstant pipe-to " > ")
(defconstant priebe "priebe_beuckelmann_1998")

(defun wrap-double-quotes (s)
  (str "\"" s "\""))

(defun .cellml (n)
  (wrap-double-quotes (str "" n ".cellml")))

(defun .cu (n)
  (wrap-double-quotes (str n ".cu")))

(defun .cpp (n)
  (wrap-double-quotes (str n ".cpp")))

(defun .out (n)
  (wrap-double-quotes (str n ".out")))

(defun compile-model! (model)
  (sh "python ../ConvertCellModel.py " (.cellml model) " -tOdeint")
  (sh "mv " (.cpp model) " " (.cu model))
  (sh "nvcc -o " (.out model) " " (.cu model) include-odeint))

(defun use-host! ()
  (print "Configuring translator to use host now.")
  (sh "sed -i 's/USE_DEVICE = True/USE_DEVICE = False/g' " translators.py))

(defun use-device! ()
  (print "Configuring translator to use device now.")
  (sh "sed -i 's/USE_DEVICE = False/USE_DEVICE = True/g' " translators.py))

(defun set-num-instances! (num)
  (print (str "Configuring translator to solve for " num " instances"))
  (sh "sed -i 's/INSTANCES = .*$/INSTANCES = " num "/g' " translators.py))

(defun run-device-instances (model num-instances output-file num-tries)
  (use-device!)
  (set-num-instances! num-instances)
  (compile-model! model)
  (average num-tries (time-sh "./" (.out model) pipe-to output-file)))

(defun run-experiment! (model)
  (let ((time-5-instances (run-device-instances model 5 "out-5.csv" 5))
        (time-128-instances (run-device-instances model 128 "out-128.csv"))
        (time-512-instances (run-device-instances model 512 "out-512.csv")))
    (print (str "8 instances took: " time-5-instances " seconds."))
    (print (str "128 instances took: " time-128-instances " seconds."))
    (print (str "512 instances took: " time-512-instances " seconds."))))

(defun reb ()
  (load "benchmarks.lisp"))
