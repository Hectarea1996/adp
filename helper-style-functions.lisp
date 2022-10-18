

(in-package :adppvt)

(adp:header "Style-maker helper functions/macros" style-helper-header)

;; ----- helper-helper-functions -----

(defun documentationp (expr)
  (stringp expr))

(defun declarationp (expr)
  (and (listp expr)
       (eq (car expr) 'declare)))

(defun check-component-symbols (symbols components)
  (loop for sym in symbols
	do (unless (symbolp sym)
	     (error "Expected a symbol. Found: ~s" sym))
	   (unless (member sym components :test #'string=)
	     (error "Expected one of the following symbols: ~s~%Found: ~s" components sym))))

(defmacro def-with-components (name &rest components)
  (let ((name-arg (intern (concatenate 'string "WITH-" (symbol-name name) "-COMPONENTS"))))
    (with-gensyms (component-rest-args function-body-arg body-arg function-body-value-var let-bindings-var lambda-var)
      `(adp:defmacro ,name-arg (((&rest ,component-rest-args) ,function-body-arg) &body ,body-arg)
       (check-component-symbols ,component-rest-args ',components)
       (let* ((,function-body-value-var (gensym))
	      (,let-bindings-var (mapcar (lambda (,lambda-var)
					   (list ,lambda-var (list (find-symbol (concatenate 'string ,(symbol-name name) "-" (symbol-name ,lambda-var)) :adppvt) ,function-body-value-var)))
					 ,component-rest-args)))
	 `(let ((,,function-body-value-var ,,function-body-arg))
	    (let ,,let-bindings-var
	      ,@,body-arg)))))))


(adp:subheader "API function components" api-components-subheader)

;; ----- defclass components -----

(adp:subsubheader "Defclass components" defclass-components-subsubheader)

(adp:defun defclass-class-name (defclass-source)
  (cadr defclass-source))

(adp:defun defclass-superclass-names (defclass-source)
  (caddr defclass-source))

(adp:defun defclass-slot-specifiers (defclass-source)
  (cadddr defclass-source))

(adp:defun defclass-slot-names (defclass-source)
  (let ((slot-specifiers (defclass-slot-specifiers defclass-source)))
    (and slot-specifiers
	 (loop for slot-specifier in slot-specifiers
	       collect (if (listp slot-specifier)
			   (car slot-specifier)
			   slot-specifier)))))

(adp:defun defclass-slot-options (defclass-source)
  (let ((slot-specifiers (defclass-slot-specifiers defclass-source)))
    (and slot-specifiers
	 (loop for slot-specifier in slot-specifiers
	       collect (if (listp slot-specifier)
			   (cdr slot-specifier)
			   nil)))))

(adp:defun defclass-reader-function-names (defclass-source)
  (let ((slot-options (defclass-slot-options defclass-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (loop for rest-slot-option on slot-option by #'cddr
				  if (eq (car rest-slot-option) :reader)
				    collect (cadr rest-slot-option)))))))

(adp:defun defclass-writer-function-names (defclass-source)
  (let ((slot-options (defclass-slot-options defclass-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (loop for rest-slot-option on slot-option by #'cddr
				  if (eq (car rest-slot-option) :writer)
				    collect (cadr rest-slot-option)))))))

(adp:defun defclass-accessor-function-names (defclass-source)
  (let ((slot-options (defclass-slot-options defclass-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (loop for rest-slot-option on slot-option by #'cddr
				  if (eq (car rest-slot-option) :accessor)
				    collect (cadr rest-slot-option)))))))

(adp:defun defclass-allocation-types (defclass-source)
  (let ((slot-options (defclass-slot-options defclass-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (cadr (member :allocation slot-option)))))))

(adp:defun defclass-initarg-names (defclass-source)
  (let ((slot-options (defclass-slot-options defclass-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (loop for rest-slot-option on slot-option by #'cddr
				  if (eq (car rest-slot-option) :initarg)
				    collect (cadr rest-slot-option)))))))

(adp:defun defclass-initforms (defclass-source)
  (let ((slot-options (defclass-slot-options defclass-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (cadr (member :initform slot-option)))))))

(adp:defun defclass-type-specifiers (defclass-source)
  (let ((slot-options (defclass-slot-options defclass-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (cadr (member :type slot-option)))))))

(adp:defun defclass-slot-documentations (defclass-source)
  (let ((slot-options (defclass-slot-options defclass-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (cadr (member :documentation slot-option)))))))

(adp:defun defclass-class-options (defclass-source)
  (cdddr defclass-source))

(adp:defun defclass-default-initargs (defclass-source)
  (let ((class-options (defclass-class-options defclass-source)))
    (and class-options
	 (cdr (member :default-initargs class-options :key #'car)))))

(adp:defun defclass-documentation (defclass-source)
  (let ((class-options (defclass-class-options defclass-source)))
    (and class-options
	 (cadr (member :documentation class-options :key #'car)))))

(adp:defun defclass-metaclass (defclass-source)
  (let ((class-options (defclass-class-options defclass-source)))
    (and class-options
	 (cadr (member :metaclass class-options :key #'car)))))

(def-with-components defclass class-name superclass-names slot-specifiers slot-names slot-options
  reader-function-names writer-function-names accessor-function-names allocation-types
  initarg-names initforms type-specifiers slot-documentations class-options default-initargs
  documentation metaclass)


;; ----- defconstant components -----

(adp:subsubheader "Defconstant components" defconstant-components-subsubheader)

(adp:defun defconstant-name (defconstant-source)
  (cadr defconstant-source))

(adp:defun defconstant-initial-value (defconstant-source)
  (caddr defconstant-source))

(adp:defun defconstant-documentation (defconstant-source)
  (cadddr defconstant-source))

(def-with-components defconstant name initial-value documentation)


;; ----- defgeneric components -----

(adp:subsubheader "Defgeneric components" defgeneric-components-subsubheader)

(adp:defun defgeneric-function-name (defgeneric-source)
  (cadr defgeneric-source))

(adp:defun defgeneric-gf-lambda-list (defgeneric-source)
  (caddr defgeneric-source))

(adp:defun defgeneric-options (defgeneric-source)
  (remove-if (lambda (x)
	       (eq (car x) :method))
	     (cdddr defgeneric-source)))

(adp:defun defgeneric-argument-precedence-order (defgeneric-source)
  (let ((options (defgeneric-options defgeneric-source)))
    (and options
	 (cdr (member :argument-precedence-order options :key #'car)))))

(adp:defun defgeneric-gf-declarations (defgeneric-source)
  (let ((options (defgeneric-options defgeneric-source)))
    (and options
	 (cdr (member 'cl:declare options :key #'car)))))

(adp:defun defgeneric-documentation (defgeneric-source)
  (let ((options (defgeneric-options defgeneric-source)))
    (and options
	 (cadr (member :documentation options :key #'car)))))

(adp:defun defgeneric-method-combination (defgeneric-source)
  (let ((options (defgeneric-options defgeneric-source)))
    (and options
	 (cdr (member :method-combination options :key #'car)))))

(adp:defun defgeneric-generic-function-class (defgeneric-source)
  (let ((options (defgeneric-options defgeneric-source)))
    (and options
	 (cadr (member :generic-function-class options :key #'car)))))

(adp:defun defgeneric-method-class (defgeneric-source)
  (let ((options (defgeneric-options defgeneric-source)))
    (and options
	 (cadr (member :method-class options :key #'car)))))

(adp:defun defgeneric-method-descriptions (defgeneric-source)
  (remove-if-not (lambda (x)
		   (eq (car x) :method))
		 (cdddr defgeneric-source)))

(def-with-components defgeneric function-name gf-lambda-list options argument-precedence-order gf-declarations
  documentation method-combination generic-function-class method-class method-descriptions)


;; ----- define-compiler-macro components -----

(adp:subsubheader "Define-compiler-macro components" define-compiler-macro-components-subsubheader)

(adp:defun define-compiler-macro-name (define-compiler-macro-source)
  (cadr define-compiler-macro-source))

(adp:defun define-compiler-macro-lambda-list (define-compiler-macro-source)
  (caddr define-compiler-macro-source))

(adp:defun define-compiler-macro-declarations (define-compiler-macro-source)
  (let ((post-lambda-list (cdddr define-compiler-macro-source)))
    (loop for expr in post-lambda-list
	  while (or (declarationp expr)
		    (documentationp expr))
	  when (declarationp expr)
	    collect expr)))

(adp:defun define-compiler-macro-documentation (define-compiler-macro-source)
  (let ((post-lambda-list (cdddr define-compiler-macro-source)))
    (loop for expr in post-lambda-list
	  while (or (declarationp expr)
		    (documentationp expr))
	  when (documentationp expr)
	    return expr)))

(adp:defun define-compiler-macro-forms (define-compiler-macro-source)
  (let ((post-lambda-list (cdddr define-compiler-macro-source)))
    (loop for expr on post-lambda-list
	  while (or (declarationp (car expr))
		    (documentationp (car expr)))
	  finally (return expr))))

(def-with-components define-compiler-macro name lambda-list declarations documentation forms)


;; ----- define-condition components -----

(adp:subsubheader "Define-condition components" define-condition-components-subsubheader)

(adp:defun define-condition-name (define-condition-source)
  (cadr define-condition-source))

(adp:defun define-condition-parent-types (define-condition-source)
  (caddr define-condition-source))

(adp:defun define-condition-slot-specs (define-condition-source)
  (cadddr define-condition-source))

(adp:defun define-condition-slot-names (define-condition-source)
  (let ((slot-specs (define-condition-slot-specs define-condition-source)))
    (and slot-specs
	 (loop for slot-spec in slot-specs
	       if (symbolp slot-spec)
		 collect slot-spec
	       else
		 collect (car slot-spec)))))

(adp:defun define-condition-slot-options (define-condition-source)
  (let ((slot-specs (define-condition-slot-specs define-condition-source)))
    (and slot-specs
	 (loop for slot-spec in slot-specs
	       if (symbolp slot-spec)
		 collect nil
	       else
		 collect (cdr slot-spec)))))

(adp:defun define-condition-slot-readers (define-condition-source)
  (let ((slot-options (define-condition-slot-options define-condition-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (loop for rest-slot-option on slot-option by #'cddr
				  if (eq (car rest-slot-option) :reader)
				    collect (cadr rest-slot-option)))))))

(adp:defun define-condition-slot-writers (define-condition-source)
  (let ((slot-options (define-condition-slot-options define-condition-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (loop for rest-slot-option on slot-option by #'cddr
				  if (eq (car rest-slot-option) :writer)
				    collect (cadr rest-slot-option)))))))

(adp:defun define-condition-slot-accessors (define-condition-source)
  (let ((slot-options (define-condition-slot-options define-condition-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (loop for rest-slot-option on slot-option by #'cddr
				  if (eq (car rest-slot-option) :accessor)
				    collect (cadr rest-slot-option)))))))

(adp:defun define-condition-slot-allocations (define-condition-source)
  (let ((slot-options (define-condition-slot-options define-condition-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (cadr (member :allocation slot-option)))))))

(adp:defun define-condition-slot-initargs (define-condition-source)
  (let ((slot-options (define-condition-slot-options define-condition-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (loop for rest-slot-option on slot-option by #'cddr
				  if (eq (car rest-slot-option) :initarg)
				    collect (cadr rest-slot-option)))))))

(adp:defun define-condition-slot-initforms (define-condition-source)
  (let ((slot-options (define-condition-slot-options define-condition-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (cadr (member :initform slot-option)))))))

(adp:defun define-condition-slot-types (define-condition-source)
  (let ((slot-options (define-condition-slot-options define-condition-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (and slot-option
			    (cadr (member :type slot-option)))))))

(adp:defun define-condition-options (define-condition-source)
  (cddddr define-condition-source))

(adp:defun define-condition-default-initargs (define-condition-source)
  (let ((options (define-condition-options define-condition-source)))
    (and options
	 (cdr (member :default-initargs options :key #'car)))))

(adp:defun define-condition-documentation (define-condition-source)
  (let ((options (define-condition-options define-condition-source)))
    (and options
	 (cadr (member :documentation options :key #'car)))))

(adp:defun define-condition-report-name (define-condition-source)
  (let ((options (define-condition-options define-condition-source)))
    (and options
	 (cadr (member :report options :key #'car)))))

(def-with-components define-condition name parent-types slot-specs slot-names slot-options slot-readers
  slot-writers slot-accessors slot-allocations slot-initargs slot-initforms slot-types options
  default-initargs documentation report-name)


;; ----- define-method-combination components -----

(adp:subsubheader "Define-method-combination components" define-method-combination-components-subsubheader)

(adp:defun define-method-combination-name (define-method-combination-source)
  (cadr define-method-combination-source))

(adp:defun define-method-combination-short-form-options (define-method-combination-source)
  (let ((possible-short-form-options (cddr define-method-combination-source)))
    (if (keywordp (car possible-short-form-options))
	possible-short-form-options
	nil)))

(adp:defun define-method-combination-identity-with-one-argument (define-method-combination-source)
  (let ((short-form-options (define-method-combination-short-form-options define-method-combination-source)))
    (and short-form-options
	 (cadr (member :identity-with-one-argument short-form-options)))))

(adp:defun define-method-combination-operator (define-method-combination-source)
  (let ((short-form-options (define-method-combination-short-form-options define-method-combination-source)))
    (and short-form-options
	 (cadr (member :operator short-form-options)))))

(adp:defun define-method-combination-lambda-list (define-method-combination-source)
  (caddr define-method-combination-source))

(adp:defun define-method-combination-method-group-specifiers (define-method-combination-source)
  (cadddr define-method-combination-source))

(adp:defun define-method-combination-method-group-specifier-names (define-method-combination-source)
  (let ((method-group-specifiers (define-method-combination-method-group-specifiers define-method-combination-source)))
    (and method-group-specifiers
	 (mapcar #'car method-group-specifiers))))

(adp:defun define-method-combination-qualifier-patterns (define-method-combination-source)
  (let ((method-group-specifiers (define-method-combination-method-group-specifiers define-method-combination-source)))
    (and method-group-specifiers
	 (loop for method-group-specifier in method-group-specifiers
	       collect (loop for possible-qualifier-pattern in (cdr method-group-specifier)
			     while (listp possible-qualifier-pattern)
			     collect possible-qualifier-pattern)))))

(adp:defun define-method-combination-predicates (define-method-combination-source)
  (let ((method-group-specifiers (define-method-combination-method-group-specifiers define-method-combination-source)))
    (and method-group-specifiers
	 (loop for method-group-specifier in method-group-specifiers
	       collect (let ((possible-predicate (cadr method-group-specifier)))
			 (if (listp possible-predicate)
			     nil
			     possible-predicate))))))

(adp:defun define-method-combination-long-form-options (define-method-combination-source)
  (let ((method-group-specifiers (define-method-combination-method-group-specifiers define-method-combination-source)))
    (and method-group-specifiers
	 (loop for method-group-specifier in method-group-specifiers
	       collect (if (symbolp (cadr method-group-specifier))
			   (cddr method-group-specifier)
			   (loop for rest-method-group-specifier on (cdr method-group-specifier)
				 while (listp (car rest-method-group-specifier))
				 finally (return rest-method-group-specifier)))))))

(adp:defun define-method-combination-descriptions (define-method-combination-source)
  (let ((long-form-options (define-method-combination-long-form-options define-method-combination-source)))
    (and long-form-options
	 (loop for long-form-option in long-form-options
	       collect (cadr (member :description long-form-option))))))

(adp:defun define-method-combination-orders (define-method-combination-source)
  (let ((long-form-options (define-method-combination-long-form-options define-method-combination-source)))
    (and long-form-options
	 (loop for long-form-option in long-form-options
	       collect (cadr (member :order long-form-option))))))

(adp:defun define-method-combination-requireds (define-method-combination-source)
  (let ((long-form-options (define-method-combination-long-form-options define-method-combination-source)))
    (and long-form-options
	 (loop for long-form-option in long-form-options
	       collect (cadr (member :required long-form-option))))))

(adp:defun define-method-combination-arguments (define-method-combination-source)
  (let ((possible-arguments (car (cddddr define-method-combination-source))))
    (if (and (listp possible-arguments)
	     (eq (car possible-arguments) :arguments))
	(cdr possible-arguments)
	nil)))

(adp:defun define-method-combination-generic-function (define-method-combination-source)
  (let ((rest-possible-generic-function (cddddr define-method-combination-source)))
    (if (and (listp (car rest-possible-generic-function))
	     (eq (caar rest-possible-generic-function) :generic-function))
	(cadar rest-possible-generic-function)
	(let ((last-possible-generic-function (cadr rest-possible-generic-function)))
	  (if (and (listp last-possible-generic-function)
		   (eq (car last-possible-generic-function) :generic-function))
	      (cadr last-possible-generic-function))))))

(adp:defun define-method-combination-declarations (define-method-combination-source)
  (loop for possible-declaration in (cddddr define-method-combination-source)
	while (or (declarationp possible-declaration)
		  (documentationp possible-declaration)
		  (and (listp possible-declaration)
		       (member (car possible-declaration) '(:arguments :generic-function))))
	when (declarationp possible-declaration)
	  collect possible-declaration))

(adp:defun define-method-combination-documentation (define-method-combination-source)
  (let ((short-form-options (define-method-combination-short-form-options define-method-combination-source)))
    (if short-form-options
	(cadr (member :documentation short-form-options))
	(loop for possible-documentation in (cdddr define-method-combination-source)
	      while (or (declarationp possible-documentation)
			(documentationp possible-documentation)
			(and (listp possible-documentation)
			     (member (car possible-documentation) '(:arguments :generic-function))))
	      when (documentationp possible-documentation)
		return possible-documentation))))

(adp:defun define-method-combination-forms (define-method-combination-source)
  (loop for possible-forms on (cddddr define-method-combination-source)
	while (or (declarationp (car possible-forms))
		  (documentationp (car possible-forms))
		  (and (listp (car possible-forms))
		       (member (caar possible-forms) '(:arguments :generic-function))))
	finally (return possible-forms)))

(def-with-components define-method-combination name short-form-options identity-with-one-argument operator
  lambda-list method-group-specifiers method-group-specifier-names qualifier-patterns predicates
  long-form-options descriptions orders requireds arguments generic-function declarations documentation forms)


;; ----- define-modify-macro components -----

(adp:subsubheader "Define-modify-macro components" define-modify-macro-components-subsubheader)

(adp:defun define-modify-macro-name (define-modify-macro-source)
  (cadr define-modify-macro-source))

(adp:defun define-modify-macro-lambda-list (define-modify-macro-source)
  (caddr define-modify-macro-source))

(adp:defun define-modify-macro-function (define-modify-macro-source)
  (cadddr define-modify-macro-source))

(adp:defun define-modify-macro-documentation (define-modify-macro-source)
  (car (cddddr define-modify-macro-source)))

(def-with-components define-modify-macro name lambda-list function documentation) 


;; ----- define-setf-expander components -----

(adp:subsubheader "Define-setf-expander components" define-setf-expander-components-subsubheader)

(adp:defun define-setf-expander-access-fn (define-setf-expander-source)
  (cadr define-setf-expander-source))

(adp:defun define-setf-expander-lambda-list (define-setf-expander-source)
  (caddr define-setf-expander-source))

(adp:defun define-setf-expander-declarations (define-setf-expander-source)
  (let ((post-lambda-list (cdddr define-setf-expander-source)))
    (loop for expr in post-lambda-list
	  while (or (declarationp expr)
		    (documentationp expr))
	  when (declarationp expr)
	    collect expr)))

(adp:defun define-setf-expander-documentation (define-setf-expander-source)
  (let ((post-lambda-list (cdddr define-setf-expander-source)))
    (loop for expr in post-lambda-list
	  while (or (declarationp expr)
		    (documentationp expr))
	  when (documentationp expr)
	    return expr)))

(adp:defun define-setf-expander-forms (define-setf-expander-source)
  (let ((post-lambda-list (cdddr define-setf-expander-source)))
    (loop for expr on post-lambda-list
	  while (or (declarationp (car expr))
		    (documentationp (car expr)))
	  finally (return expr))))

(def-with-components define-setf-expander access-fn lambda-list declarations documentation forms)


;; ----- define-symbol-macro components -----

(adp:subsubheader "Define-symbol-macro components" define-symbol-macro-components-subsubheader)

(adp:defun define-symbol-macro-symbol (define-symbol-macro-source)
  (cadr define-symbol-macro-source))

(adp:defun define-symbol-macro-expansion (define-symbol-macro-source)
  (caddr define-symbol-macro-source))

(def-with-components define-symbol-macro symbol expansion)


;; ----- defmacro components -----

(adp:subsubheader "Defmacro components" defmacro-components-subsubheader)

(adp:defun defmacro-name (defmacro-source)
  (cadr defmacro-source))

(adp:defun defmacro-lambda-list (defmacro-source)
  (caddr defmacro-source))

(adp:defun defmacro-declarations (defmacro-source)
  (let ((post-lambda-list (cdddr defmacro-source)))
    (loop for expr in post-lambda-list
	  while (or (declarationp expr)
		    (documentationp expr))
	  when (declarationp expr)
	    collect expr)))

(adp:defun defmacro-documentation (defmacro-source)
  (let ((post-lambda-list (cdddr defmacro-source)))
    (loop for expr in post-lambda-list
	  while (or (declarationp expr)
		    (documentationp expr))
	  when (documentationp expr)
	    return expr)))

(adp:defun defmacro-forms (defmacro-source)
  (let ((post-lambda-list (cdddr defmacro-source)))
    (loop for expr on post-lambda-list
	  while (or (declarationp (car expr))
		    (documentationp (car expr)))
	  finally (return expr))))

(def-with-components defmacro name lambda-list declarations documentation forms)


;; ----- defmethod components -----

(adp:subsubheader "defmethod components" defmethod-components-subsubheader)

(adp:defun defmethod-function-name (defmethod-source)
  (cadr defmethod-source))

(adp:defun defmethod-method-qualifiers (defmethod-source)
  (loop for qualifier in (cddr defmethod-source)
	while (not (listp qualifier))
	collect qualifier))

(adp:defun defmethod-specialized-lambda-list (defmethod-source)
  (loop for possible-lambda-list in (cddr defmethod-source)
	when (listp possible-lambda-list)
	  return possible-lambda-list))

(adp:defun defmethod-declarations (defmethod-source)
  (loop for rest-defmethod-source on (cddr defmethod-source)
	when (listp (car rest-defmethod-source))
	  return (loop for possible-declaration in (cdr rest-defmethod-source)
		       while (or (declarationp possible-declaration)
				 (documentationp possible-declaration))
		       if (declarationp possible-declaration)
			 collect possible-declaration)))

(adp:defun defmethod-documentation (defmethod-source)
  (loop for rest-defmethod-source on (cddr defmethod-source)
	when (listp (car rest-defmethod-source))
	  return (loop for possible-documentation in (cdr rest-defmethod-source)
		       while (or (declarationp possible-documentation)
				 (documentationp possible-documentation))
		       if (documentationp possible-documentation)
			 return possible-documentation)))

(adp:defun defmethod-forms (defmethod-source)
  (loop for rest-defmethod-source on (cddr defmethod-source)
	when (listp (car rest-defmethod-source))
	  return (loop for rest-possible-forms on (cdr rest-defmethod-source)
		       while (or (declarationp (car rest-possible-forms))
				 (documentationp (car rest-possible-forms)))
		       finally (return rest-possible-forms))))

(def-with-components defmethod function-name method-qualifiers specialized-lambda-list declarations
  documentation forms)


;; ----- defpackage components -----

(adp:subsubheader "Defpackage components" defpackage-components-subsubheader)

(adp:defun defpackage-options (defpackage-source)
  (cddr defpackage-source))

(adp:defun defpackage-defined-package-name (defpackage-source)
  (cadr defpackage-source))

(adp:defun defpackage-nicknames (defpackage-source)
  (let ((options (defpackage-options defpackage-source)))
    (mapcan #'cdr (remove-if-not (lambda (x)
				   (eq (car x) :nicknames))
				 options))))

(adp:defun defpackage-documentation (defpackage-source)
  (let ((options (defpackage-options defpackage-source)))
    (and options
	 (cadr (member :documentation options)))))

(adp:defun defpackage-use-package-names (defpackage-source)
  (let ((options (defpackage-options defpackage-source)))
    (mapcan #'cdr (remove-if-not (lambda (x)
				   (eq (car x) :use))
				 options))))

(adp:defun defpackage-shadow-symbol-names (defpackage-source)
  (let ((options (defpackage-options defpackage-source)))
    (mapcan #'cdr (remove-if-not (lambda (x)
				   (eq (car x) :shadow))
				 options))))

(adp:defun defpackage-shadowing-import-from-package-names (defpackage-source)
  (let ((options (defpackage-options defpackage-source)))
    (mapcar #'cadr (remove-if-not (lambda (x)
				   (eq (car x) :shadowing-import-from))
				  options))))

(adp:defun defpackage-shadowing-import-from-symbol-names (defpackage-source)
  (let ((options (defpackage-options defpackage-source)))
    (mapcan #'cddr (remove-if-not (lambda (x)
				   (eq (car x) :shadowing-import-from))
				 options))))

(adp:defun defpackage-import-from-package-names (defpackage-source)
  (let ((options (defpackage-options defpackage-source)))
    (mapcar #'cadr (remove-if-not (lambda (x)
				   (eq (car x) :import-from))
				  options))))

(adp:defun defpackage-import-from-symbol-names (defpackage-source)
  (let ((options (defpackage-options defpackage-source)))
    (mapcan #'cddr (remove-if-not (lambda (x)
				   (eq (car x) :import-from))
				  options))))

(adp:defun defpackage-export-symbol-names (defpackage-source)
  (let ((options (defpackage-options defpackage-source)))
    (mapcan #'cdr (remove-if-not (lambda (x)
				   (eq (car x) :export))
				 options))))

(adp:defun defpackage-import-symbol-names (defpackage-source)
  (let ((options (defpackage-options defpackage-source)))
    (mapcan #'cdr (remove-if-not (lambda (x)
				   (eq (car x) :export))
				 options))))

(adp:defun defpackage-size (defpackage-source)
  (let ((options (defpackage-options defpackage-source)))
    (cadar (member :size options :key #'car))))

(def-with-components defpackage options defined-package-name nicknames documentation use-package-names
  shadow-symbol-names shadowing-import-from-package-names shadowing-import-from-symbol-names
  import-from-package-names import-from-symbol-names export-symbol-names import-symbol-names size)


;; ----- defparameter components -----

(adp:subsubheader "Defparameter components" defparameter-components-subsubheader)

(adp:defun defparameter-name (defparameter-source)
  (cadr defparameter-source))

(adp:defun defparameter-initial-value (defparameter-source)
  (caddr defparameter-source))

(adp:defun defparameter-documentation (defparameter-source)
  (cadddr defparameter-source))

(def-with-components defparameter name initial-value documentation)


;; ----- defsetf components -----

(adp:subsubheader "Defsetf components" defsetf-components-subsubheader)

(adp:defun defsetf-access-fn (defsetf-source)
  (cadr defsetf-source))

(adp:defun defsetf-update-fn (defsetf-source)
  (let ((possible-update-fn (caddr defsetf-source)))
    (if (symbolp possible-update-fn)
	possible-update-fn)))

(adp:defun defsetf-documentation (defsetf-source)
  (cadddr defsetf-source))

(def-with-components defsetf access-fn update-fn documentation)


;; ----- defstruct -----

(adp:subsubheader "Defstruct components" defstruct-components-subsubheader)

(adp:defun defstruct-name-and-options (defstruct-source)
  (cadr defstruct-source))

(adp:defun defstruct-structure-name (defstruct-source)
  (let ((name-and-options (defstruct-name-and-options defstruct-source)))
    (if (listp name-and-options)
	(car name-and-options)
	name-and-options)))

(adp:defun defstruct-options (defstruct-source)
  (let ((name-and-options (defstruct-name-and-options defstruct-source)))
    (if (listp name-and-options)
	(cdr name-and-options)
	nil)))

(adp:defun defstruct-conc-name-option (defstruct-source)
  (let ((options (defstruct-options defstruct-source)))
    (if (not (null options))
	(or (car (member :conc-name options))
	    (car (member :conc-name options :key #'car)))
	nil)))

(adp:defun defstruct-constructor-options (defstruct-source)
  (let ((options (defstruct-options defstruct-source)))
    (remove-if-not (lambda (x)
		     (if (listp x)
			 (eq (car x) :constructor)
			 (eq x :constructor)))
		   options)))

(adp:defun defstruct-copier-option (defstruct-source)
  (let ((options (defstruct-options defstruct-source)))
    (if (not (null options))
	(or (car (member :copier options))
	    (car (member :copier options :key #'car)))
	nil)))

(adp:defun defstruct-predicate-option (defstruct-source)
  (let ((options (defstruct-options defstruct-source)))
    (if (not (null options))
	(or (car (member :predicate options))
	    (car (member :predicate options :key #'car)))
	nil)))

(adp:defun defstruct-include-option (defstruct-source)
  (let ((options (defstruct-options defstruct-source)))
    (if (not (null options))
	(or (car (member :include options))
	    (car (member :include options :key #'car)))
	nil)))

(adp:defun defstruct-printer-option (defstruct-source)
  (let ((options (defstruct-options defstruct-source)))
    (if (not (null options))
	(or (car (member :print-object options :key #'car))
	    (car (member :print-function options :key #'car)))
	nil)))

(adp:defun defstruct-print-object-option (defstruct-source)
  (let ((printer-option (defstruct-printer-option defstruct-source)))
    (if (and (not (null printer-option))
	     (eq (car printer-option) :print-object))
	printer-option
	nil)))

(adp:defun defstruct-print-function-option (defstruct-source)
  (let ((printer-option (defstruct-printer-option defstruct-source)))
    (if (and (not (null printer-option))
	     (eq (car printer-option) :print-function))
	printer-option
	nil)))

(adp:defun defstruct-type-option (defstruct-source)
  (let ((options (defstruct-options defstruct-source)))
    (if (not (null options))
	(car (member :type options :key #'car))
	nil)))

(adp:defun defstruct-named-option (defstruct-source)
  (let ((options (defstruct-options defstruct-source)))
    (if (not (null options))
	(car (member :named options))
	nil)))

(adp:defun defstruct-initial-offset-option (defstruct-source)
  (let ((options (defstruct-options defstruct-source)))
    (if (not (null options))
	(car (member :initial-offset options :key #'car))
	nil)))

(adp:defun defstruct-slot-descriptions (defstruct-source)
  (let ((possible-descriptions (cddr defstruct-source)))
    (if (documentationp (car possible-descriptions))
	(cdr possible-descriptions)
	possible-descriptions)))

(adp:defun defstruct-slot-options (defstruct-source)
  (let ((slot-descriptions (defstruct-slot-descriptions defstruct-source)))
    (loop for slot-description in slot-descriptions
	  collect (if (listp slot-description)
		      (cddr slot-description)
		      nil))))

(adp:defun defstruct-conc-name (defstruct-source)
  (let ((conc-name-option (defstruct-conc-name-option defstruct-source)))
    (and conc-name-option
	 (if (listp conc-name-option)
	     (cadr conc-name-option)
	     nil))))

(adp:defun defstruct-constructor-arglists (defstruct-source)
  (let ((constructor-options (defstruct-constructor-options defstruct-source)))
    (and constructor-options
	 (loop for constructor-option in constructor-options
	       collect (if (listp constructor-option)
			   (caddr constructor-option)
			   nil)))))

(adp:defun defstruct-constructor-names (defstruct-source)
  (let ((constructor-options (defstruct-constructor-options defstruct-source)))
    (and constructor-options
	 (loop for constructor-option in constructor-options
	       collect (if (listp constructor-option)
			   (cadr constructor-option)
			   nil)))))

(adp:defun defstruct-copier-name (defstruct-source)
  (let ((copier-option (defstruct-copier-option defstruct-source)))
    (and copier-option
	 (if (listp copier-option)
	     (cadr copier-option)
	     nil))))

(adp:defun defstruct-included-structure-name (defstruct-source)
  (let ((include-option (defstruct-include-option defstruct-source)))
    (and include-option
	 (cadr include-option))))

(adp:defun defstruct-include-option-slot-descriptors (defstruct-source)
  (let ((include-option (defstruct-include-option defstruct-source)))
    (and include-option
	 (cddr include-option))))

(adp:defun defstruct-initial-offset (defstruct-source)
  (let ((initial-offset-option (defstruct-initial-offset-option defstruct-source)))
    (and initial-offset-option
	 (cadr initial-offset-option))))

(adp:defun defstruct-predicate-name (defstruct-source)
  (let ((predicate-option (defstruct-predicate-option defstruct-source)))
    (and predicate-option
	 (cadr predicate-option))))

(adp:defun defstruct-printer-name (defstruct-source)
  (let ((printer-option (defstruct-printer-option defstruct-source)))
    (and printer-option
	 (cadr printer-option))))

(adp:defun defstruct-slot-names (defstruct-source)
  (let ((slot-descriptions (defstruct-slot-descriptions defstruct-source)))
    (and slot-descriptions
	 (loop for slot-description in slot-descriptions
	       collect (if (listp slot-description)
			   (car slot-description)
			   slot-description)))))

(adp:defun defstruct-slot-initforms (defstruct-source)
  (let ((slot-descriptions (defstruct-slot-descriptions defstruct-source)))
    (and slot-descriptions
	 (loop for slot-description in slot-descriptions
	       collect (if (listp slot-description)
			   (cadr slot-description)
			   nil)))))

(adp:defun defstruct-slot-types (defstruct-source)
  (let ((slot-options (defstruct-slot-options defstruct-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (cadr (member :type slot-option))))))

(adp:defun defstruct-slot-read-only-ps (defstruct-source)
  (let ((slot-options (defstruct-slot-options defstruct-source)))
    (and slot-options
	 (loop for slot-option in slot-options
	       collect (cadr (member :read-only slot-option))))))

(adp:defun defstruct-type (defstruct-source)
  (let ((type-option (defstruct-type-option defstruct-source)))
    (and type-option
	 (cadr type-option))))

(adp:defun defstruct-documentation (defstruct-source)
  (let ((possible-documentation (caddr defstruct-source)))
    (when (documentationp possible-documentation)
      possible-documentation)))

(def-with-components defstruct name-and-options options conc-name-option constructor-option
  copier-option predicate-option include-option printer-option print-object-option
  print-function-option type-option named-option initial-offset-option slot-descriptions
  slot-options conc-name constructor-arglists constructor-names copier-name
  included-structure-name include-option-slot-descriptions initial-offset predicate-name
  printer-name slot-names slot-initforms slot-read-only-ps slot-types structure-name type
  documentation)


;; ----- deftype components -----

(adp:subsubheader "Deftype components" deftype-components-subsubheader)

(adp:defun deftype-name (deftype-source)
  (cadr deftype-source))

(adp:defun deftype-lambda-list (deftype-source)
  (caddr deftype-source))

(adp:defun deftype-declarations (deftype-source)
  (let ((post-lambda-list (cdddr deftype-source)))
    (loop for expr in post-lambda-list
	  while (or (declarationp expr)
		    (documentationp expr))
	  when (declarationp expr)
	    collect expr)))

(adp:defun deftype-documentation (deftype-source)
  (let ((post-lambda-list (cdddr deftype-source)))
    (loop for expr in post-lambda-list
	  while (or (declarationp expr)
		    (documentationp expr))
	  when (documentationp expr)
	    return expr)))

(adp:defun deftype-forms (deftype-source)
  (let ((post-lambda-list (cdddr deftype-source)))
    (loop for expr on post-lambda-list
	  while (or (declarationp (car expr))
		    (documentationp (car expr)))
	  finally (return expr))))

(def-with-components deftype name lambda-list declarations documentation forms)


;; ----- defun components -----

(adp:subsubheader "Defun components" defun-components-subsubheader)

(adp:defun defun-function-name (defun-source)
  (cadr defun-source))

(adp:defun defun-lambda-list (defun-source)
  (caddr defun-source))

(adp:defun defun-declarations (defun-source)
  (let ((post-lambda-list (cdddr defun-source)))
    (loop for expr in post-lambda-list
	  while (or (declarationp expr)
		    (documentationp expr))
	  when (declarationp expr)
	    collect expr)))

(adp:defun defun-documentation (defun-source)
  (let ((post-lambda-list (cdddr defun-source)))
    (loop for expr in post-lambda-list
	  while (or (declarationp expr)
		    (documentationp expr))
	  when (documentationp expr)
	    return expr)))

(adp:defun defun-forms (defun-source)
  (let ((post-lambda-list (cdddr defun-source)))
    (loop for expr on post-lambda-list
	  while (or (declarationp (car expr))
		    (documentationp (car expr)))
	  finally (return expr))))

(def-with-components defun function-name lambda-list declarations documentation forms)


;; ----- defvar components -----

(adp:subsubheader "Defvar components" defvar-components-subsubheader)

(adp:defun defvar-name (defvar-source)
  (cadr defvar-source))

(adp:defun defvar-initial-value (defvar-source)
  (caddr defvar-source))

(adp:defun defvar-documentation (defvar-source)
  (cadddr defvar-source))

(def-with-components defvar name initial-value documentation)


(adp:write-in-file #P"docs/style-maker-help")
