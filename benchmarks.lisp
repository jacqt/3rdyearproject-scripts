(defmacro e (&rest args)
  `(ext:run-shell-command ,@args))

(defun read-stream (s)
  (loop for line = (read-line s nil)
        while line
        collect line))

(defmacro sh (&rest args)
  "Runs a shell command and returns a string representation of the shell output."
  `(let ((command (str ,@args)))
     (with-open-stream (s (e command :output :stream))
       (read-stream s))))

(defmacro time-sh (&rest args)
  "Runs a shell command and times the computation."
  `(let ((start-time (get-internal-real-time)))
     (sh ,@args)
     (float (infix (((get-internal-real-time) - start-time) / internal-time-units-per-second)))))

(defconstant home-dir "/home/ug13ag2/")
(defconstant chaste-py-dir (str home-dir "Chaste/python/"))
(defconstant translators.py (str chaste-py-dir "pycml/translators.py"))
(defconstant include-odeint (str " -I " home-dir "odeint/include"))
(defconstant disable-opt (str " -Xptxas -O0 -Xcompiler -O0"))
(defconstant enable-gprof (str " -Xcompiler \"-g -pg\""))
(defconstant enable-double-precision " -arch sm_13 ")
(defconstant pipe-to " > ")
(defconstant suppress-warnings " 2> /dev/null")
(defconstant priebe "priebe_beuckelmann_1998")
(defconstant hodgkin "hodgkin_huxley_squid_axon_model_1952_modified")
(defconstant circle "circle_ode")
(defconstant profile-me "export CUDA_PROFILE=1 && export CUDA_PROFILE_CONFIG=config.txt && export CUDA_PROFILE_LOG=profile.dat && ")

(defun wrap-double-quotes (s)
  (str "\"" s "\""))

(defun .cellml (n)
  (wrap-double-quotes (str n ".cellml")))

(defun .cu (n)
  (wrap-double-quotes (str n ".cu")))

(defun .cpp (n)
  (wrap-double-quotes (str n ".cpp")))

(defun .hpp (n)
  (wrap-double-quotes (str n ".hpp")))

(defun .out (n)
  (wrap-double-quotes (str n ".out")))


(defun compile-model! (model)
  (print "Compiling model now.")
  (sh "python ../ConvertCellModel.py " (.cellml model) " -tOdeint"  suppress-warnings)
  (sh "mv " (.cpp model) " " (.cu model))
  (sh "nvcc -o " (.out model) " " (.cu model) include-odeint enable-double-precision enable-gprof  ))

(defun clean-up! (model)
  (sh "rm " (.hpp model))
  (sh "rm " (.cu model))
  (sh "rm " (.out model)))

(defun set-parameter-study! ()
  (print "Configuring translator to do a parameter study now.")
  (sh "sed -i 's/PARAMETER_STUDY = False/PARAMETER_STUDY = True/g' " translators.py))

(defun unset-parameter-study! ()
  (print "Configuring translator to not do a parameter study now.")
  (sh "sed -i 's/PARAMETER_STUDY = True/PARAMETER_STUDY = False/g' " translators.py))

(defun use-host! ()
  (print "Configuring translator to use host now.")
  (sh "sed -i 's/USE_DEVICE = True/USE_DEVICE = False/g' " translators.py))

(defun use-device! ()
  (print "Configuring translator to use device now.")
  (sh "sed -i 's/USE_DEVICE = False/USE_DEVICE = True/g' " translators.py))

(defun set-num-instances! (num)
  (print (str "Configuring translator to solve for " num " instances now."))
  (sh "sed -i 's/INSTANCES = .*$/INSTANCES = " num "/g' " translators.py))

(defun set-absolute-tolerance! (abs-tolerance)
  (print (str "Configuring translator to use an absolute tolerance of " abs-tolerance " now."))
  (sh "sed -i 's/ABS_TOLERANCE = .*$/ABS_TOLERANCE = \"" abs-tolerance "\"/g' " translators.py))

(defun set-relative-tolerance! (rel-tolerance)
  (print (str "Configuring translator to use a relative  tolerance of " rel-tolerance " now."))
  (sh "sed -i 's/REL_TOLERANCE = .*$/REL_TOLERANCE = \"" rel-tolerance "\"/g' " translators.py))

(defun use-forward-euler! ()
  (print (str "Configuring translator to use the forward euler solver."))
  (sh "sed -i 's/SOLVER = .*$/SOLVER = \"ForwardEuler\"/g' " translators.py) )

(defun use-rk4! ()
  (print (str "Configuring translator to use the classic runge kutta solver."))
  (sh "sed -i 's/SOLVER = .*$/SOLVER = \"Rk4\"/g' " translators.py))

(defun use-dopri5! ()
  (print (str "Configuring translator to use the Dormand Prince 5 adaptive solver."))
  (sh "sed -i 's/SOLVER = .*$/SOLVER = \"Dopri5\"/g' " translators.py))

(defun set-const-timestep! (ts)
  (print (str "Configuring translator to use a constant timestep size of " ts))
  (sh "sed -i 's/CONSTANT_TIMESTEP = .*$/CONSTANT_TIMESTEP = " ts "/g' " translators.py))

(defun run-binary! (model num-tries output-dir)
  (loop for  i from 0 below num-tries
        while (< i 5)
        collect (progn
                  (print (str "Experiment run " i))
                  (princ (time-sh "./" (.out model) pipe-to "./" output-dir "/run-" i "-output.csv")))))


(defun run-device-instances (model num-instances output-dir num-tries)
  (print (str "Running experiment: " output-dir))
  (use-device!)
  (set-num-instances! num-instances)
  (compile-model! model)
  (sh "mkdir -p " output-dir suppress-warnings)
  (run-binary! model num-tries output-dir))

(defun run-device-experiment! (model)
  (let ((time-1-instances (run-device-instances model 1 "output/priebe-device-1" 1))
        (time-4-instances (run-device-instances model 4 "output/priebe-device-4" 1))
        (time-8-instances (run-device-instances model 8 "output/priebe-device-8" 1))
        (time-16-instances (run-device-instances model 16 "output/priebe-device-16" 1))
        (time-32-instances (run-device-instances model 32 "output/priebe-device-32" 1))
        (time-48-instances (run-device-instances model 48 "output/priebe-device-48" 1)))
    (print (str "1 instances took: " time-1-instances " seconds."))
    (print (str "4 instances took: " time-4-instances " seconds."))
    (print (str "8 instances took: " time-8-instances " seconds."))
    (print (str "16 instances took: " time-16-instances " seconds."))
    (print (str "32 instances took: " time-32-instances " seconds."))
    (print (str "48 instances took: " time-48-instances " seconds."))
    (clean-up! model)))

(defun run-host-instances (model num-instances output-dir num-tries)
  (print (str "Running experiment: " output-dir))
  (use-host!)
  (set-num-instances! num-instances)
  (compile-model! model)
  (sh "mkdir -p " output-dir suppress-warnings)
  (run-binary! model num-tries output-dir))

(defun run-host-experiment! (model)
  (unset-parameter-study!)
  (let ((time-1-instances (run-host-instances model 1 "output/priebe-host-1" 3))
        (time-4-instances (run-host-instances model 4 "output/priebe-host-4" 3))
        (time-8-instances (run-host-instances model 8 "output/priebe-host-8" 3))
        (time-16-instances (run-host-instances model 16 "output/priebe-host-16" 3))
        (time-32-instances (run-host-instances model 32 "output/priebe-host-32" 3))
        (time-48-instances (run-host-instances model 48 "output/priebe-host-48" 3)))
    (print (str "1 instances took: " time-1-instances " seconds."))
    (print (str "4 instances took: " time-4-instances " seconds."))
    (print (str "8 instances took: " time-8-instances " seconds."))
    (print (str "16 instances took: " time-16-instances " seconds."))
    (print (str "32 instances took: " time-32-instances " seconds."))
    (print (str "48 instances took: " time-48-instances " seconds."))
    (clean-up! model)))


(defun run-circle-experiment ()
  ; (use-rk4!)
  (use-forward-euler!)
  (loop for i from -1 to 10
        for ts = (float (infix (10 expt ((- i) / 2))))
        for dirname = (str "output/circle-error/" i)
        collect (progn
                  (print (str "Solving with timestep " ts))
                  (set-const-timestep! ts)
                  (run-host-instances circle 1 dirname 1))))

(defun run-adaptive-circle-experiment ()
  (use-dopri5!)
  (loop for i from -5 to 5
        for ts = (float (infix (10 expt ((- i) / 2))))
        for dirname = (str "output/circle-error/" i)
        collect (progn
                  (print (str "Solving with absolute & relative error of " ts))
                  (set-absolute-tolerance! ts)
                  (set-relative-tolerance! ts)
                  (run-host-instances circle 1 dirname 1))))

(defun reb ()
  (load "benchmarks.lisp"))
