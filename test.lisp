;; THE TIMESTEP TO SAMPLE THE VALUES AT
(defconstant +SAMPLE-RATE+ 1)

(defun match? (pair)
  (destructuring-bind (a b) pair
    (equal a b)))

(defun headers-match? (table1 table2)
  (let ((headers-1 (first table1))
        (headers-2 (first table2)))
    (if (= (length headers-1) (length headers-2))
      (every 'match?  (zip headers-1 headers-2)))))

(defun valid-header? (table)
  (let ((header (first table)))
    (equal (first header) "t")))

(defun valid-tables? (table1 table2)
  (and
    (headers-match? table1 table2)
    (valid-header? table1)
    (valid-header? table2)))

(defun get-time (row)
  (first row))

(defun get-point (row)
  (rest row))

(defun after? (sample-time zipped-row)
  (> sample-time (get-time (second zipped-row))))

(defun interpolate (zipped-rows sample-time)
  (destructuring-bind (first-row second-row) (first zipped-rows)
    (if (after? sample-time (get-time zipped-rows))
      (interpolate (rest zipped-rows) sample-time)
      (let ((timestep (- (get-time second-row) (get-time first-row)))
            (sample-part (- sample-time (get-time first-row))))
        (let ((multiplier (/ sample-part timestep)))
          (map
            'list
            (fn ((v1 v2))
                (infix (v1 + (multiplier * (v2 - v1)))))
            (zip first-row second-row)))))))

;; function used in the fold for calculating all samples
(defun compute-next-sample (acc next-sample-time)
  (destructuring-bind (prev-samples zipped-rows) acc
    (list (cons (interpolate zipped-rows next-sample-time) prev-samples)
          (drop-while (& 'after? next-sample-time) zipped-rows))))

(defun sample-values (rows max-sample-value)
  (let ((zipped-rows (zip rows (rest rows)))
        (sample-times (range 0 max-sample-value +SAMPLE-RATE+)))
    (->
      (foldr
        'compute-next-sample
        (list () zipped-rows)
        sample-times)
      (first)
      (reverse))))

(defun squared-distance (vs)
  (destructuring-bind (v1 v2) vs
    (infix ((v1 - v2) * (v1 - v2)))))

(defun euclidean-distance (points)
  (destructuring-bind (p1 p2) points
    (sqrt
      (foldr
        (fn (err vs) (+ err (squared-distance vs)))
        0
        (zip p1 p2)))))

(defun compute-error (values-1 values-2)
  (print "Computing errors...")
  ;; First compute the maximimal timestep we can sample using linear interpolation
  (let ((max-t (min (apply 'max (map 'list 'first values-1))
                    (apply 'max (map 'list 'first values-2)))))
    ;; Sample the datapoints in increments of .5ms
    (let ((sampled-values-1 (sample-values values-1 max-t))
          (sampled-values-2 (sample-values values-2 max-t)))
      ;; Compute the difference in the samples
      (let ((zipped-sample (zip sampled-values-1 sampled-values-2)))
        (foldr
          (fn (total-error points) (+ total-error (euclidean-distance points)))
          0
          zipped-sample)))))

;; handle float underflow case when parsing expressions like "1e-50"
;; by parsing to infinite precision rational numbers
(defun parse-rational (input)
  (let ((splitted (utils:split-sequence #\e input)))
    (if (< (length splitted) 2)
      (rationalize (read-from-string input))
      (destructuring-bind (factor exponent) splitted
        (* (parse-rational factor) (expt 10 (parse-rational exponent)))))))

(defun get-values (table)
  (map
    'list
    (fn (row) (map 'list 'parse-rational row))
    (rest table)))

(defun run (table1 table2)
  (print "Determining if files are compatible...")
  (if (not (valid-tables? table1 table2))
    (print "ERROR: The two data files are incompatible")
    (progn
      (print "Extracting values from csv files...")
      (compute-error (get-values table1) (get-values table2)))))

(defun main (fname1 fname2)
  (let ((table1 (cl-csv:read-csv fname1))
        (table2 (cl-csv:read-csv fname2)))
    (run table1 table2)))

(defun test-me ()
  (main #P"./out-5.csv" #P"./out-5-b.csv"))
