
(in-package :addpvt)


;; -----------------------
;; ----- select-file -----
;; -----------------------

(defun project-add-file (project file)
  "Add a file into a project."
  (declare (type project project) (type file file))
  (with-slots (files) project
    (vector-push-extend file files)))

(defun project-find-file (project path)
  "Return a file whose path is PATH. If there is not a file with path PATH, return NIL."
  (declare (type project project) (type pathname path))
  (with-slots (files) project
    (loop for file across files
	  if (equal path (slot-value file 'path))
	    return file
	  finally (return nil))))

(defun select-file (project path)
  (with-slots (root-directory) project
    (let* ((complete-path (merge-pathnames path root-directory))
	   (file (or (project-find-file project path)
		    (let ((new-file (make-instance 'file :path complete-path)))
		      (project-add-file project new-file)
		      new-file))))
      (with-slots (current-file) project
	(setf current-file file)))))


;; ------------------------
;; ----- add-code-tag -----
;; ------------------------

(defun add-code-tag (tag code)
  "Associate a tag with a piece of code."
  (delcare (type symbol tag) (type code code))
  (tag-table-push-element *code-tags* tag code))


;; -------------------------------------
;; ----- project-relative-pathname -----
;; -------------------------------------

(defun relative-truename (project &optional (path *load-truename*))
  "Return the *load-truename* path relative to the project root directory."
  (declare (type project project))
  (with-slots (root-directory) project
    (let* ((root-level (length (cdr (pathname-directory root-directory))))
	   (relative-path (make-pathname :directory (cons :relative (nthcdr (1+ root-level) (pathname-directory path)))
					 :name (pathname-name path)
					 :type (pathname-type path))))
      (values relative-path))))


;; -----------------------
;; ----- add-element -----
;; -----------------------

(defgeneric check-subelements (element)
  (:documentation
   "Check the use of subelements."))

(defmethod check-subelements ((element element))
  (values))

(defmethod check-subelements ((element (or text cell)))
  (with-slots (text-elements) element
    (loop for text-element in text-elements
	  when (and (typep text-element 'element)
		    (not (typep text-element 'web-link)))
	    do (if (typep text-element 'text-enrichment)
		   (check-subelements text-element)
		   (error 'text-subelement-error :text element :subelement text-element)))))

(defmethod check-subelements ((element text-type))
  (with-slots (text-elements) element
    (loop for text-element in text-elements
	  when (typep text-element 'element)
	    do (error 'text-subelement-error :text element :subelement text-element))))

(defmethod check-subelements ((element table))
  (with-slots (rows) element
    (loop for row in rows
	  do (loop for cell-element in row
		   if (typep cell-element 'cell)
		     do (check-subelements cell-element)
		   else
		     do (error 'table-subelement-error :table element :subelement cell-element)))))

(defmethod check-subelements ((element itemize-type))
  (with-slots ((items elements)) element
    (when (null items)
      (error 'null-itemize-error :itemize element))
    (when (not (typep (car items) 'item))
      (error 'itemize-first-element-not-item-error :itemize element :subelement (car items)))
    (loop for item in (cdr items)
	  if (typep item '(or itemize-type item))
	    do (check-subelements item)
	  else
	    do (error 'itemize-subelement-error :itemize element :subelement item))))


(defun file-push-element (file element)
  "Add an element in a file."
  (declare (type file file) (type element element))
  (with-slots (elements) file
    (vector-push-extend element elements)))


(defgeneric add-element (project element)
  (:documentation
   "Add an element into a project."))

(defmethod add-element (project (element element))
  (with-slots (current-file) project
    (when (not current-file)
      (error 'file-not-selected-error :first-element element))
    (check-subelements element)
    (setf (slot-value element 'file-location) current-file)
    (file-push-element current-file element)))

(defmethod add-element :after (project (element header-type))
  (with-slots (tag (header-location source-location)) element 
    (multiple-value-bind (previous-header foundp) (tag-table-find-elements *header-tags* tag)
      (if foundp
	  (error 'already-defined-tag-error :source-element element :previous-source-element previous-header :tag tag)
	  (tag-table-set-element *header-tags* tag element)))))


(defmethod add-element :after (project (element table-of-contents))
  (setf (slot-value element 'project) project))

(defmethod add-element :after (project (element file-table-of-contents))
  (with-slots (current-file) project
    (setf (slot-value element 'file) current-file)))


(defmethod add-element :after (project (element symbol-definition))
  (with-slots (tag) element
    (tag-table-set-element *symbol-tags* tag element)))

(defmethod add-element :after (project (element function-definition))
  (with-slots (tag) element
    (tag-table-set-element *function-tags* tag element)))

(defmethod add-element :after (project (element type-definition))
  (with-slots (tag) element
    (tag-table-set-element *type-tags* tag element)))


;; ----------------------
;; ----- file-print -----
;; ----------------------

(defun file-print (file)
  "Create a documentation file and prints the documentation in it."
  (declare (type file file))
  (with-slots (elements path) file
    (let ((complete-path (merge-pathnames path (make-pathname :type (funcall *file-extension*)))))
      (ensure-directories-exist complete-path :verbose nil)
      (with-open-file (stream complete-path :direction :output :if-does-not-exist :create :if-exists :supersede)
	(when *file-head-writer*
	  (funcall *file-head-writer* stream))
	(loop for element in elements
	      do (element-print element stream))
	(when *file-foot-writer*
	  (funcall *file-foot-writer* stream))))))


;; -------------------------
;; ----- project-print -----
;; -------------------------

(defun project-print (project)
  "Generate a project documentation."
  (declare (type project project))
  (with-slots (files root-directory) project
    (when *general-files-writer*
      (funcall *general-files-writer* root-directory))
    (loop for file across files
	  do (format "Printing file '~a'.~%" (relative-truename (slot-value file 'path)))
	     (file-print file))))


;; -------------------------
;; ----- element-print -----
;; -------------------------

(defgeneric element-print (element stream)
  (:documentation
   "Print an element into the stream."))

;; header
(defmethod element-print ((element header) stream)
  (declare (special *header-writer*))
  (with-slots (title tag) element
    (funcall *header-writer* stream title tag)))

(defmethod element-print ((element subheader) stream)
  (declare (special *subheader-writer*))
  (with-slots (title tag) element
    (funcall *subheader-writer* stream title tag)))

(defmethod element-print ((element subsubheader) stream)
  (declare (special *subsubheader-writer*))
  (with-slots (title tag) element
    (funcall *subsubheader-writer* stream title tag)))

;; text
(defun text-type-to-string (text)
  "Turn a text element into a string."
  (declare (type text-type text))
  (with-slots (text-elements) text
    (let ((processed-elements (mapcar (lambda (text-element)
					(if (typep text-element 'element)
					    (with-output-to-string (str-stream)
					      (element-print text-element str-stream))
					    (when *excape-text*
					      (funcall *escape-text* text-element))))
				      text-elements)))
      (apply #'concatenate 'string processed-elements))))

(defmethod element-print ((element text) stream)
  (let ((text-elements (slot-value element 'text-elements)))
    (funcall *text-writer* stream (text-type-to-string element))))

;; text enrichment
(defmethod element-print ((element bold) stream)
  (funcall *bold-writer* stream (text-type-to-string element)))

(defmethod element-print ((element italic) stream)
  (funcall *italic-writer* stream (text-type-to-string element)))

(defmethod element-print ((element bold-italic) stream)
  (funcall *bold-italic-writer* stream (text-type-to-string element)))

(defmethod element-print ((element code-inline) stream)
  (funcall *code-inline-writer* stream (text-type-to-string element)))

;; text reference
(defmethod element-print ((element header-ref) stream)
  (with-slots (tag) element
    (multiple-value-bind (associated-elements element-found-p) (tag-table-find-elements *header-tags* tag)
      (if element-found-p
	  (with-slots (file-location title) (aref associated-elements 0)
	    (funcall *header-ref-writer* stream tag title file-location))
	  (error 'tag-not-defined-error :source-element element :tag tag)))))

(defmethod element-print ((element symbol-ref) stream)
  (with-slots (tag) element
    (multiple-value-bind (associated-elements element-found-p) (tag-table-find-elements *symbol-tags* tag)
      (if element-found-p
	  (with-slots (file-location) (aref associated-elements 0)
	    (funcall *symbol-ref-writer* stream tag file-location))
	  (error 'tag-not-defined-error :source-element element :tag tag)))))

(defmethod element-print ((element function-ref) stream)
  (with-slots (tag) element
    (multiple-value-bind (associated-elements element-found-p) (tag-table-find-elements *function-tags* tag)
      (if element-found-p
	  (with-slots (file-location) (aref associated-elements 0)
	    (funcall *function-ref-writer* stream tag file-location))
	  (error 'tag-not-defined-error :source-element element :tag tag)))))

(defmethod element-print ((element type-ref) stream)
  (with-slots (tag) element
    (multiple-value-bind (associated-elements element-found-p) (tag-table-find-elements *type-tags* tag)
      (if element-found-p
	  (with-slots (file-location) (aref associated-elements 0)
	    (funcall *type-ref-writer* stream tag file-location))
	  (error 'tag-not-defined-error :source-element element :tag tag)))))

;; web link
(defmethod element-print ((element web-link) stream)
  (with-slots (text address) element
    (funcall *web-link-writer* stream text address)))

;; image
(defmethod element-print ((element image) stream)
  (funcall *image-writer* stream (slot-value element 'path)))

;; table
(defmethod element-print ((element table) stream)
  (let ((processed-table (loop for row in rows
			       collect (mapcar #'text-type-to-string row))))
    (funcall *table-writer* stream processed-table)))

;; itemize
(defgeneric process-itemize (element)
  (:documentation
   "Turn an itemize element into a style-maker suitable representation.")

  (:method ((element item))
    (list :item (text-type-to-string element)))

  (:method ((element itemize))
    (with-slots (elements type) element
      (let ((processed-elements (mapcar #'process-itemize elements)))
	(list* :itemize processed-elements))))

  (:method ((element enumerate))
    (with-slots (elements type) element
      (let ((processed-elements (mapcar #'process-itemize elements)))
	(list* :enumerate processed-elements)))))

(defmethod element-print ((element itemize) stream)
  (funcall *itemize-writer* stream (process-itemize element)))


;; table-of-contents
(defun file-headers (file)
  "Return the header-type elements of a file."
  (declare (type file file))
  (let ((headers (make-array 10 :adjustable t :fill-pointer 0 :element-type 'header-type)))
    (with-slots (elements) file
      (loop for element across element
	    when (typep element 'header-type)
	      do (vector-push-extent element headers)))
    (values headers)))

(defun project-headers (project)
  "Return the header-type elements of a project."
  (declare (type project project))
  (let ((headers (make-array 10 :adjustable t :fill-pointer 0 :element-type 'header-type)))
    (with-slots (files) project
      (loop for file across files
	    do (let ((file-headers (file-headers file)))
		 (loop for file-header across file-headers
		       if (typep file-header '(or header subheader))
			 do (vector-push-extend file-header headers)))))
    (values headers)))

(defun header-deep-level (header)
  "Return the level of deepness of a header."
  (declare (type header-type header))
  (typecase header
    (header 0)
    (subheader 1)
    (subsubheader 2)
    (t (error "The object ~s is not a header-type element." header))))

(defun make-toc-deep-levels (headers)
  "Return a vector of deepness levels the headers must have in a table of contents."
  (declare (type (vector element) header))
  (let ((deep-levels (make-array 100 :adjustable t :fill-pointer 0 :element-type 'unsigned-byte)))
    (loop for header across headers
	  for prev-min-deep-level = 2 then next-min-deep-level
	  for prev-deep-level =     2 then next-deep-level
	  for (next-min-deep-level next-deep-level) = (let ((header-deep-level (header-deep-level header)))
							(cond
							  ((> header-deep-level prev-deep-level)
							   (let ((next-deep-level (1+ prev-deep-level)))
							     (list prev-min-deep-level next-deep-level)))
							  ((< header-deep-level prev-deep-level)
							   (if (>= header-deep-level prev-min-deep-level)
							       (list prev-min-deep-level (- header-deep-level prev-min-deep-level))
							       (list header-deep-level 0)))
							  (t
							   (list prev-min-deep-level header-deep-level))))
	  do (vector-push-extend next-deep-level deep-levels))
    (values deep-levels)))

(defun make-itemize-toc (source-element headers)
  (with-slots (source-location) source-element
    (let* ((deep-levels (create-toc-deep-levels headers))
	   (total-deep-levels (length deep-levels))
	   (index 0))
      (labels ((make-itemize-toc-aux (current-level)
		 (loop while (< index total-deep-levels)
		       for header = (aref headers index)
		       for deep-level = (aref deep-levels index)
		       until (< deep-level current-level)
		       if (> deep-level current-level)
			 collect (make-instance 'itemize
						:name "itemize"
						:elements (make-itemize-toc-aux (1+ current-level))
						:source-location source-location)
			   into toc-list
		       else
			 collect (make-instance 'item
						:name "item"
						:text-elements (list (make-instance 'header-ref
										    :name "header-ref"
										    :tag (slot-value header 'tag)
										    :source-location source-location))
						:source-location source-location)
			   into toc-list
			   and do (incf index)
		       finally (return toc-list))))
	(make-instance 'itemize
		       :name "itemize"
		       :elements (make-itemize-toc-aux 0)
		       :source-location source-location)))))

(defmethod element-print ((element table-of-contents) stream)
  (with-slots (project) element
    (let ((headers (project-headers project)))
      (funcall *itemize-writer* (process-itemize (make-itemize-toc element headers))))))

(defmethod element-print ((element mini-table-of-contents) stream)
  (with-slots (file) element
    (let ((headers (file-headers file)))
      (funcall *itemize-writer* (process-itemize (make-itemize-toc element headers))))))


;; table-of-function/symbols/types
(defun split-ordered-symbols (symbols)
  )

(defun make-itemize-tof (source-element)
  (with-slots (source-location) source-element
    (let ((functions-list (sort (tag-table-tags *function-tags*) #'string>=))
	  (temp-list nil)
	  (items-list nil))
      (loop for function-tag in functions-list
	    for prev-letter = (aref (symbol-name function-tag) 0) then current-letter
	    for current-letter = (aref (symbol-name function-tag) 0)
	    if (equal prev-letter current-letter)
	      do (push (make-instance 'item
				      :name "item"
				      :text-elements (list (make-instance 'function-ref
									  :name "function-ref"
									  :tag function-tag
									  :source-location source-location))
				      :source-location source-location)
		       temp-list)
	    else
	      do (push (make-instance 'itemize
				      :name "itemize"
				      :elements temp-list
				      :source-location source-location)
		       items-list)
		 (push (make-instance 'item
				      :name "item"
				      :text-elements (list prev-letter)
				      :source-location source-location)
		       items-list)
		 (setf temp-list nil)
		 (push (make-instance 'item
				      :name "item"
				      :text-elements (list (make-instance 'function-ref
									  :name "function-ref"
									  :tag function-tag
									  :source-location source-location))
				      :source-location source-location)
		       temp-list)
	    finally (when temp-list
		      (push (make-instance 'itemize
					   :name "itemize"
					   :elements temp-list
					   :source-location source-location)
			    items-list)
		      (push (make-instance 'item
					   :name "item"
					   :text-elements (list current-letter)
					   :source-location source-location)
			    items-list))
		    (return (cons :itemize items-list))))))

(defun create-table-of-symbols ()
  (let ((symbols-list (sort (hash-table-keys *symbol-tags*) #'string>=))
	(temp-list nil)
	(items-list nil))
    (loop for symbol-tag in symbols-list
	  for prev-letter = (aref (symbol-name symbol-tag) 0) then current-letter
	  for current-letter = (aref (symbol-name symbol-tag) 0)
	  if (equal prev-letter current-letter)
	    do (push `(:item ,(create-symbol-ref-text symbol-tag)) temp-list)
	  else
	    do (push `(:itemize ,@temp-list) items-list)
	    and do (push `(:item ,prev-letter) items-list)
	    and do (setf temp-list nil)
	    and do (push `(:item ,(create-symbol-ref-text symbol-tag)) temp-list)
	  finally (when temp-list
		    (push `(:itemize ,@temp-list) items-list)
		    (push `(:item ,current-letter) items-list))
		  (return (cons :itemize items-list)))))

(defun create-table-of-types ()
  (let ((types-list (sort (hash-table-keys *type-tags*) #'string>=))
	(temp-list nil)
	(items-list nil))
    (loop for type-tag in types-list
	  for prev-letter = (aref (symbol-name type-tag) 0) then current-letter
	  for current-letter = (aref (symbol-name type-tag) 0)
	  if (equal prev-letter current-letter)
	    do (push `(:item ,(create-type-ref-text type-tag)) temp-list)
	  else
	    do (push `(:itemize ,@temp-list) items-list)
	    and do (push `(:item ,prev-letter) items-list)
	    and do (setf temp-list nil)
	    and do (push `(:item ,(create-type-ref-text type-tag)) temp-list)
	  finally (when temp-list
		    (push `(:itemize ,@temp-list) items-list)
		    (push `(:item ,current-letter) items-list))
		  (return (cons :itemize items-list)))))


;; code
(defun shortest-string (strings)
  (loop for str in strings
	for shortest = str then (if (< (length str) (length shortest))
				    str
				    shortest)
	finally (return shortest)))

(defun make-custom-symbol-pprint-function (hide-symbol hide-str)
  (lambda (stream sym)
    (if (eq sym hide-symbol)
	(format stream hide-str)
	(let* ((sym-package (symbol-package sym))
	       (nickname (and sym-package
			      (shortest-string (package-nicknames sym-package))))
	       (print-package-mode (and sym-package
					(not (equal sym-package (find-package "CL")))
					(case (nth-value 1 (find-symbol (symbol-name sym) sym-package))
					  (:external :external)
					  (:internal (if (or (boundp sym) (fboundp sym))
							 :internal
							 nil))
					  (t nil))))
	       (package-to-print (and print-package-mode
				      (or nickname
					  (and (keywordp sym) "")
					  (package-name sym-package))))
	       (*print-escape* nil)
	       (*print-pprint-dispatch* normal-pprint-dispatch))
	  (case print-package-mode
	    (:external (format stream "~a:~a" package-to-print (symbol-name sym)))
	    (:internal (format stream "~a::~a" package-to-print (symbol-name sym)))
	    (t (format stream "~a" (symbol-name sym))))))))

(defun comment-pprint (stream code-comment)
  (format stream ";; ~a" (cadr code-comment)))

(defun make-custom-pprint-dispatch (hide-symbol hide-str comment-symbol)
  (let ((normal-pprint-dispatch *print-pprint-dispatch*)
	(custom-pprint-dispatch (copy-pprint-dispatch)))    
    (set-pprint-dispatch '(cons (member adp:defclass)) (pprint-dispatch '(defclass)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:defconstant)) (pprint-dispatch '(defconstant)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:defgeneric)) (pprint-dispatch '(defgeneric)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:define-compiler-macro)) (pprint-dispatch '(define-compiler-macro)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:define-condition)) (pprint-dispatch '(define-condition)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:define-method-combination)) (pprint-dispatch '(define-method-combination)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:define-modify-macro)) (pprint-dispatch '(define-modify-macro)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:define-setf-expander)) (pprint-dispatch '(define-setf-expander)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:define-symbol-macro)) (pprint-dispatch '(define-symbol-macro)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:defmacro)) (pprint-dispatch '(defmacro)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:defmethod)) (pprint-dispatch '(defmethod)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:defpackage)) (pprint-dispatch '(defpackage)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:defparameter)) (pprint-dispatch '(defparameter)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:defsetf)) (pprint-dispatch '(defsetf)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:defstruct)) (pprint-dispatch '(defstruct)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:deftype)) (pprint-dispatch '(deftype)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:defun)) (pprint-dispatch '(defun)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch '(cons (member adp:defvar)) (pprint-dispatch '(defvar)) 0 custom-pprint-dispatch)
    (set-pprint-dispatch 'symbol (make-custom-symbol-pprint-function hide-symbol hide-str) 0 custom-pprint-dispatch)
    (set-pprint-dispatch `(cons (member ,comment-symbol)) #'comment-pprint 0 custom-pprint-dispatch)))

(defun code-to-string (code)
  "Turn a code element into a string."
  (declare (type code code))
  (with-slots (expr hide-symbol comment-symbol) code
    (let ((custom-pprint-dispatch (make-custom-pprint-dispatch hide-symbol hide-str comment-symbol)))
      (let ((*print-pprint-dispatch* custom-pprint-dispatch))
	(with-output-to-string (stream)
	  (prin1 expr stream))))))

(defmethod element-print ((element code-block) stream)
  (with-slots (code-type code-elements) element
    (let ((complete-code-elements (mapcan (lambda (code-or-ref)
					    (if (typep code-or-ref 'code-reference)
						(with-slots (tag) code-or-ref
						  (multiple-value-bind (tag-code-elements tag-foundp) (tag-table-find-elements *code-tags* tag)
						    (if tag-foundp
							(coerce tag-code-elements 'list)
							(error 'tag-not-defined-error :source-element element :tag tag))))
						(list code-or-ref)))
					  code-elements))
	  (code-string (format nil "~{~a~^~%~%~}" (mapcar #'code-to-string complete-code-elements))))
      (funcall *code-block-writer* stream code-type code-string))))

(defmethod element-print ((element verbatim-code-block) stream)
  (with-slots (code-type code-text) element
    (funcall *code-block-writer* stream code-type code-text)))

(defmethod element-print ((element code-example) stream)
  (with-slots (code output result) element
    (let ((code-string (format nil "~{~a~^~%~%~}" (mapcar #'code-to-string code-elements))))
      (funcall *code-example-writer* stream code output result))))

;; definition
(defmacro define-definition-element-print (type writer)
  (with-gensyms (element stream expr)
    `(defmethod element-print ((,element ,type) ,stream)
       (with-slots (,expr) ,element
	 (funcall ,writer ,stream ,expr)))))

(define-definition-element-print defclass *defclass-writer*)
(define-definition-element-print defconstant *defconstant-writer*)
(define-definition-element-print defgeneric *defgeneric-writer*)
(define-definition-element-print define-compiler-macro *define-compiler-macro-writer*)
(define-definition-element-print define-condition *define-condition-writer*)
(define-definition-element-print define-method-combination *define-method-combination-writer*)
(define-definition-element-print define-modify-macro *define-modify-macro-writer*)
(define-definition-element-print define-setf-expander *define-setf-expander-writer*)
(define-definition-element-print define-symbol-macro *define-symbol-macro-writer*)
(define-definition-element-print defmacro *defmacro-writer*)
(define-definition-element-print defmethod *defmethod-writer*)
(define-definition-element-print defpackage *defpackage-writer*)
(define-definition-element-print defparameter *defparameter-writer*)
(define-definition-element-print defsetf *defsetf-writer*)
(define-definition-element-print defstruct *defstruct-writer*)
(define-definition-element-print deftype *deftype-writer*)
(define-definition-element-print defun *defun-writer*)
(define-definition-element-print defvar *defvar-writer*)

