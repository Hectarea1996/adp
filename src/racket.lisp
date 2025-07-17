
(in-package #:adp)


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defclass racket-arg ()
    ((name :initform nil :initarg :name)
     (optional :initform nil :initarg :optional)
     (default :initform nil :initarg :default)
     (supplied :initform nil :initarg :supplied)))

  (defmethod make-load-form ((object racket-arg) &optional env)
    (declare (ignore env))
    (with-slots (name default supplied) object
      `(make-instance 'racket-arg :name ',name :default ',default, :supplied ',supplied)))

  (cl:defun parse-racket-arg (#1=#:arg requires-default-form &aux (arg (alexandria:ensure-list #1#)))
    (let ((size (length arg))
          (arg-obj (make-instance 'racket-arg)))
      (unless (and (> size 0)
                   (< size 4))
        (error "Expected a symbol or a list with the form (symbol [form [symbol]]) but found: ~s" #1#))
      (let ((arg1 (first arg)))
        (if (symbolp arg1)
            (setf (slot-value arg-obj 'name) arg1)
            (error "Expected a symbol as the name of an argument but found: ~s" arg1)))
      (if (>= size 2)
          (with-slots (optional default) arg-obj
            (setf optional t)
            (setf default (second arg)))
          (when requires-default-form
            (error "Found, after an optional argument, the required argument: ~s" (first arg))))
      (when (>= size 3)
        (let ((arg3 (third arg)))
          (if (symbolp arg3)
              (setf (slot-value arg-obj 'supplied) arg3)
              (error "Expected a symbol as the name of a suppliedp argument but found: ~s" arg3))))
      (values arg-obj)))

  (defclass racket-args ()
    ((required-args :initform (make-array 10 :fill-pointer 0)
                    :initarg :required-args)
     (optional-args :initform (make-array 10 :fill-pointer 0)
                    :initarg :optional-args)
     (key-args :initform (make-hash-table :test 'eq)
               :initarg :key-args)
     (rest-arg :initform nil
               :initarg :rest-arg)))

  (defmethod make-load-form ((object racket-args) &optional env)
    (declare (ignore env))
    (with-slots (required-args optional-args key-args rest-arg) object
      `(make-instance 'racket-args :required-args ,required-args
                                   :optional-args ,optional-args
                                   :key-args ,key-args
                                   :rest-arg ',rest-arg)))

  (cl:defun parse-racket-args (args)
    (macrolet ((it-get (it)
                 `(car ,it))
               (it-next (it)
                 `(setf ,it (cdr ,it)))
               (it-endp (it)
                 `(atom ,it)))
      (let ((racket-args (make-instance 'racket-args))
            (it args)
            (requires-default-form nil))
        (when args
          (loop
            ;; &rest parameter
            if (and (symbolp (it-get it))
                    (eql (aref (symbol-name (it-get it)) 0) #\&))
              do (cond
                   ((string= (symbol-name (it-get it)) "&REST")
                    (it-next it)
                    (let ((arg (it-get it)))
                      (if (symbolp arg)
                          (setf (slot-value racket-args 'rest-arg) arg)
                          (error "The rest parameter must be a symbol but found: ~s" arg)))
                    (it-next it)
                    (unless (it-endp it)
                      (error "Expected the end of parameters but found: ~s" (it-get it))))
                   (t (error "Expected &REST but found: ~s" (it-get it))))
            else
              ;; keyword parameter
              if (keywordp (it-get it)) 
                do (let ((key (it-get it)))
                     (it-next it)
                     (when (it-endp it)
                       (error "Expected an argument after the keyword ~s" key))
                     (let ((key-arg (it-get it)))
                       (setf (gethash key (slot-value racket-args 'key-args)) (parse-racket-arg key-arg nil))))
            else
              ;; regular parameter
              do (let* ((arg (it-get it))
                        (racket-arg (parse-racket-arg arg requires-default-form))
                        (optionalp (slot-value racket-arg 'optional))
                        (args-slot (if optionalp 'optional-args 'required-args)))
                   (when (and optionalp
                              (not requires-default-form))
                     (setf requires-default-form t))
                   (vector-push-extend racket-arg (slot-value racket-args args-slot)))
                 
            do (it-next it)
               
               ;; improper lambda list
            if (and (it-endp it)
                    (not (null it))) 
              do (if (symbolp it)
                     (setf (slot-value racket-args 'rest-arg) it)
                     (error "The rest parameter must be a symbol but found: ~s" it))
                 
            while (not (it-endp it))))
        (values racket-args))))


  (cl:defun parse-racket-values (vals racket-args)
    (macrolet ((it-get (it)
                 `(car ,it))
               (it-next (it)
                 `(setf ,it (cdr ,it)))
               (it-endp (it)
                 `(atom ,it)))
      (let ((vals-it vals))
        (if vals
            (loop
              ;; keyword value
              if (keywordp (it-get vals-it))
                append (let ((key (it-get vals-it)))
                         (unless (nth-value 1 (gethash key (slot-value racket-args 'key-args)))
                           (error "Unrecognized keyword: ~s" key))
                         (it-next vals-it)
                         (when (it-endp vals-it)
                           (error "Expected a value after the keyword: ~s" key))
                         (let ((value (it-get vals-it)))
                           (list key value)))
                  into key-values
              else
                ;; regular value
                collect (it-get vals-it)
                  into regular-values
              do (it-next vals-it)
              while (not (it-endp vals-it))
              finally (return (values key-values regular-values)))
            (values nil nil)))))

  (cl:defun function-lambda-list (symbol)
    "Retrieves the lambda list of an adp function or macro whose name is SYMBOL.
A second value is returned specifing if SYMBOL does or does not denote an adp function or macro."
    (alexandria:with-gensyms (default)
      (let ((lambda-list (get symbol 'lambda-list default)))
        (if (eq lambda-list default)
            (values nil nil)
            (values lambda-list t)))))

  (cl:defun (setf function-lambda-list) (lambda-list symbol)
    (setf (get symbol 'lambda-list) lambda-list)))

(macrolet
    ((defdef (type name)
       `(cl:defmacro ,name (name (&rest args) &body body)
          
          (let ((racket-args (parse-racket-args args)))
            (multiple-value-bind (actual-body declarations docstring) (alexandria:parse-body body :documentation t)
              (alexandria:with-gensyms (key-args-sym args-sym reg-args-sym)
                (let ((required-args (loop for required-arg across (slot-value racket-args 'required-args)
                                           collect (slot-value required-arg 'name)))
                      (optional-args (loop for optional-arg across (slot-value racket-args 'optional-args)
                                           collect (with-slots (name default supplied) optional-arg
                                                     (list* name default (alexandria:ensure-list supplied)))))
                      (key-args (loop for key being the hash-key of (slot-value racket-args 'key-args)
                                        using (hash-value value)
                                      collect (with-slots (name optional default supplied) value
                                                (list* (list key name)
                                                       (if optional
                                                           default
                                                           `(error "The key argument ~s was not supplied."
                                                                   ,key))
                                                       (alexandria:ensure-list supplied)))))
                      (rest-arg (slot-value racket-args 'rest-arg)))
                  `(progn
                     (eval-when (:compile-toplevel :load-toplevel :execute)
                       (setf (function-lambda-list ',name) ',args))
                     (,',(find-symbol (symbol-name type) "COMMON-LISP") ,name (&rest ,args-sym)
                      ,@(when docstring
                          `(,docstring))
                      (multiple-value-bind (,key-args-sym ,reg-args-sym) (parse-racket-values ,args-sym ,racket-args)
                        (destructuring-bind ((,@(when key-args
                                                  `(&key ,@key-args)))
                                             ,@required-args
                                             ,@(when optional-args
                                                 `(&optional ,@optional-args))
                                             ,@(when rest-arg
                                                 `(&rest ,rest-arg)))
                            (list* ,key-args-sym ,reg-args-sym)
                          ,@declarations
                          ,@actual-body)))))))))))
  (defdef defun defun-aux)
  (defdef defmacro defmacro-aux))

(cl:defmacro defun (name (&rest args) &body body)
  "Racket-like version of DEFUN.

This macro has the following syntax:

(defun name racket-lambda-list [[declaration* | documentation]] form*)

  racket-lambda-list ::= (arg*)
                       | (arg* . rest-symbol)
                       | (arg* &rest rest-symbol)
                 arg ::= symbol
                       | (symbol default-form [supplied-symbol])
                       | keyword symbol
                       | keyword (symbol default-form [supplied-symbol])

The following explanation was taken and modified from: https://docs.racket-lang.org/reference/lambda.html

Regarding the racket-lambda-list and considering only the first case of arg, DEFUN has one of the
following three forms:

  - (arg*)

    The function accepts as many non-keyword argument values as the number of symbols.
    Each symbol is associated with an argument value by position.

  - (arg* . rest-symbol)
  - (arg* &rest rest-symbol)

    The function accepts any number of non-keyword arguments greater or equal to the number of symbols.
    When the function is applied, the symbols are associated with argument values by position,
    and all leftover arguments are placed into a list that is associated to rest-symbol.

More generally, an arg can include a keyword and/or default value. Thus, the three cases above are
more completely specified as follows:

  - symbol

    Adds one to both the minimum and maximum number of non-keyword arguments accepted by the function.
    The symbol is associated with an actual argument by position.

  - (symbol default-form [supplied-symbol])

    Adds one to the maximum number of non-keyword arguments accepted by the procedure.
    The symbol is associated with an actual argument by position, and if no such argument is provided,
    the default-form is evaluated to produce a value associated with symbol. If supplied-symbol is
    specified, it is associated with t or nil indicating if the argument is provided.
    No arg with a default-expr can appear before a symbol without a default-form and without a keyword.

  - keyword symbol

    The function requires a keyword-based argument using keyword. The symbol is associated
    with a keyword-based actual argument using keyword.

  - keyword (symbol default-form [supplied-symbol])

    The function accepts a keyword-based argument using keyword. The symbol is associated
    with a keyword-based actual argument using keyword, if supplied in an application; otherwise,
    the default-expr is evaluated to obtain a value to associate with id. If supplied-symbol is
    specified, it is associated with t or nil indicating if the argument is provided."
  `(defun-aux ,name ,args ,@body))

(cl:defmacro defmacro (name (&rest args) &body body)
  "Same as ADP:DEFUN but arguments are not evaluated."
  `(defmacro-aux ,name ,args ,@body))
