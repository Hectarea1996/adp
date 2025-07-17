
(in-package #:adp)


;; ------ elements ------
(defclass scribble-element ()
  ((value :initarg :value)
   (form :initarg :form))
  (:documentation
   "Represents an element in a scribble file."))

(cl:defun element-value (scribble-element)
  "Returns the value of an element.

This value is what a certain form returned within a scribble file.
The form can be retrieved with ADP:ELEMENT-FORM."
  (slot-value scribble-element 'value))

(cl:defun element-form (scribble-element)
  "Returns the form of an element.

This form is the lisp form equivalent to a scribble one that the programmer wrote
within a scribble file. This form returned a value that can be retrieved with ADP:ELEMENT-VALUE."
  (slot-value scribble-element 'form))

;; ------ files ------
(defclass scribble-file ()
  ((component :initarg :component)
   (elements :initarg :elements))
  (:documentation
   "Represents a unit of documentation that groups several elements."))

(defun file-component (scribble-file)
  "Returns the ASDF component of a scribble file."
  (slot-value scribble-file 'component))

(defun file-elements (scribble-file)
  "Returns all the elements of a scribble file."
  (slot-value scribble-file 'elements))

(defvar *files* nil
  "A container with all the content files. The keys are the pathnames of the file relative to the asdf root file.")

(cl:defun make-files-container ()
  "Initializes the files container."
  (setf *files* (make-array 16 :fill-pointer 0)))

(cl:defun destroy-files-container ()
  "Destroys the files container."
  (setf *files* nil))

(cl:defun add-file (file)
  "Adds a file to the files container."
  (vector-push-extend file *files*))


;; ------ adp-system ------
(defclass adp-system (asdf:system) ())

(defmethod asdf:operation-done-p ((o asdf:prepare-op) (c adp-system))
  "We can't know where the output files will be stored. So, files must always be generated."
  nil)

(defmethod asdf:operation-done-p ((o asdf:load-op) (c adp-system))
  "We can't know where the output files will be stored. So, files must always be generated."
  nil)

(defmethod asdf:perform :after ((o asdf:prepare-op) (s adp-system))
  (make-files-container))

(defmethod asdf:perform :after ((o asdf:load-op) (s adp-system))
  (export-content s (coerce *files* 'list))
  (destroy-files-container))


;; ------ scribble-source-file ------
(defclass scribble-source-file (asdf:source-file)
  ((type :initform "scrbl"))
  (:documentation "Component class for a Scribble source file"))

(defmethod asdf:operation-done-p ((o asdf:load-op) (c scribble-source-file))
  "We can't know where the output files will be stored. So, files must always be generated."
  nil)

(defmethod asdf:perform ((o asdf:prepare-op) (c scribble-source-file))
  (values))

(defmethod asdf:perform ((o asdf:compile-op) (c scribble-source-file))
  (values))

(define-condition eval-error (error)
  ((error-condition :initarg :error-condition
                    :initform nil
                    :accessor error-condition)
   (expression :initarg :expression
               :initform nil
               :accessor expression)
   (file :initarg :file
         :initform nil
         :accessor file))
  (:report (lambda (c s)
             (format s "Error evaluating: ~s~%" (expression c))
             (format s "Error description: ~a~%" (error-condition c))
             (format s "File: '~a'~%" (file c)))))

(cl:defun eval-expression (expr file)
  (handler-case (eval expr)
    (error (c)
      (error 'eval-error :error-condition c :expression expr :file file))))

(cl:defun in-package-expr-p (expr)
  (and (listp expr)
       (eq (car expr) 'in-package)))

(cl:defun in-readtable-expr-p (expr)
  (and (listp expr)
       (eq (car expr) 'named-readtables:in-readtable)))

(cl:defun get-lisp-expressions-and-stream (file)
  (let (package-expr readtable-expr (expr-count 0))
    (with-open-file (stream file)
      (loop for expr = (ignore-errors (read stream nil))
            while expr
            do (cond
                 ((in-package-expr-p expr)
                  (setf package-expr expr))
                 ((in-readtable-expr-p expr)
                  (setf readtable-expr expr))
                 (t
                  (return)))
               (incf expr-count)))
    (let ((stream (open file)))
      (loop for i from 0 below expr-count
            do (read stream nil))
      (values package-expr readtable-expr stream))))

(cl:defmacro with-lisp-expressions-and-stream ((in-package-expr in-readtable-expr stream) file &body body)
  (alexandria:with-gensyms (stream-sym)
    `(multiple-value-bind (,in-package-expr ,in-readtable-expr ,stream)
         (get-lisp-expressions-and-stream ,file)
       (let ((,stream-sym ,stream))
         (unwind-protect
              (progn ,@body)
           (close ,stream-sym))))))

(defmethod asdf:perform ((o asdf:load-op) (c scribble-source-file))
  (let ((system (asdf:component-system c)))
    (unless (typep system 'adp-system)
      (error "The system is not a valid adp system. Did you forget to specify ':class' in your system definition?")))
  (let ((file (first (asdf:input-files o c))))
    (with-lisp-expressions-and-stream (in-package-expr in-readtable-expr stream) file
      (with-slots (default-readtable) c
        (let ((*package* *package*)
              (*readtable* (named-readtables:find-readtable default-readtable)))
          (when in-package-expr
            (eval-expression in-package-expr file))
          (when in-readtable-expr
            (eval-expression in-readtable-expr file))
          (let* ((file-content (alexandria:read-stream-content-into-string stream))
                 (expressions (cdr (read-from-string (format nil "@list{~a}" file-content))))
                 (elements (loop for expr in expressions
                                 collect (make-instance 'scribble-element
                                                        :value (eval-expression expr file)
                                                        :form expr)))
                 (content-file (make-instance 'scribble-file :component c :elements elements)))
            (add-file content-file)))))))


;; ------ exporter ------
(cl:defun reexport (symbol package)
  (import symbol package)
  (export symbol package))

(cl:defmacro define-adp-system (name)
  "Defines an adp system class.

NAME must be a symbol denoting the class of a system.
The user will need to use a keyword with the same name to specify
the defined class in its documentation system."
  (check-type name symbol)
  `(progn
     (defclass ,name (adp-system) ())
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (reexport ',name "ASDF"))))

(cl:defmacro define-adp-file (name &key (type "scrbl") (default-readtable :scribble))
  "Defines an adp file.

NAME must be a symbol denoting the class of a file.
The user will need to use a keyword with the same name to specify
the defined file in its documentation system.
TYPE denotes the extension of the file.
DEFAULT-READTABLE denotes the default readtable to use when loading
the scribble file. This can be used if the exporter defines some extension to scribble
like latex math mode."
  (check-type name symbol)
  (check-type type (or string null))
  (check-type default-readtable symbol)
  `(progn
     (defclass ,name (scribble-source-file)
       ((default-readtable :initform ',default-readtable)
        ,@(and type `((type :initform ,type)))))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (reexport ',name "ASDF"))))

(defgeneric export-content (system files)
  (:documentation
   "Exports the elements gathered by ADP.

This function must be implemented by the exporter and is called after all files are loaded.
SYSTEM argument must be specialized with a class defined with DEFINE-ADP-SYSTEM.
FILES is a list of ADP::SCRIBBLE-FILE objects. These objects has two attributes that can be retrieved
using ADP:FILE-COMPONENT and ADP:FILE-ELEMENTS. The former returns the ASDF component that represents
the file. The exporter must use the ASDF interface to retrieve information about this file like
its pathname. The latter is a list of ADP::SCRIBBLE-ELEMENT objects. Each object has two attributes
as well that can be retrieved with ADP:ELEMENT-VALUE and ADP:ELEMENT-FORM. The former returns
the value of the element. This value is what the exporter must print. The former is the form
that has generated the value. This can be useful for error messages.

Regarding the type of each file, they are specifically of type a class defined with ADP:DEFINE-ADP-FILE.
In fact, every file type defined with ADP:DEFINE-ADP-FILE has as direct superclass the
 type ADP::SCRIBBLE-FILE.

All files are in the same order as they were loaded. The same goes to elements within a file.")
  (:method (system files)
    (declare (ignore files))
    (error "The system of class ~s is not an ADP system." (class-of system))))
