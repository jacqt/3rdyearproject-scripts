(load "~/quicklisp/setup.lisp")

(ql:quickload "cl-csv") ;; for csv parsing
(ql:quickload "alexandria") ;; general utils
(ql:quickload "cl-utilities") ;; general utils

(rename-package :cl-utilities :cl-utilities :utils)
(rename-package :alexandria :alexandria :a)

;; function to make reloading easier.
(defun reload ()
  (load "test.lisp"))

(defun reinit ()
  (load "init.lisp"))

;; My language
(defmacro -> (fvalue &rest body)
  "Clojure threading -> macro"
  (if (equal nil body)
    `,fvalue
    `(-> (apply #',(first (first body)) ,fvalue ',(rest (first body))) ,@(rest body))))

(defmacro fn (args &rest body)
  "Allows destructuring in lambda terms"
  (let ((symbols (map 'list #'(lambda (arg) (gentemp)) args)))
    `(lambda
       ,symbols
       ,(foldr
          (function
            (lambda (acc-expr next-arg)
              (destructuring-bind (sym arg) next-arg
                (if (listp arg)
                  `(destructuring-bind ,arg ,sym ,acc-expr)
                  `(let ((,arg ,sym)) ,acc-expr)))))
          `(progn ,@body)
          (zip symbols `,args)))))

(defmacro infix ((lhs infix-func rhs))
  "Allows binary infix notation"
  `(,infix-func
     ,(if (listp lhs)
        `(infix ,lhs)
        lhs)
     ,(if (listp rhs)
        `(infix ,rhs)
        rhs)))

(defun & (func &rest args)
  (lambda (&rest more-args)
    (apply func (append args more-args))))


(defun foldr (reducer init-value vals)
  "Haskell prelude foldr"
  (reduce
    reducer
    (cons init-value vals)))

(defun zip (list1 list2)
  "Haskell prelude zip"
  (mapcar 'list list1 list2))

(defun drop-while (bool-func vals)
  (if (equal '() vals)
    vals
    (if (funcall bool-func (first vals))
      (drop-while bool-func (rest vals))
      vals)))

(defun range (start end inc)
  "Clojure range function"
  (if (>= start end)
    ()
    (cons start (range (+ start inc) end inc))))

(defun nil? (v)
  (equal nil v))

(reload)
