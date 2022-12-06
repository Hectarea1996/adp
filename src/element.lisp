
(in-package :adppvt)


;; ----- element -----

(defclass element ()
  ((name :initarg :name)
   (source-location :initarg :source-location
		    :type pathname)
   (file-location :initform nil
		  :type file))
  (:documentation
   "Represent the most basic unit of documentation."))

(defclass tagged-element (element)
  ((tag :initarg :tag
	:type symbol))
  (:documentation
   "Represent an element that can be associated with a tag."))


;; ----- header -----

(defclass header-type (tagged-element)
  ((title :initarg :title
	  :type string))
  (:documentation
   "Represents a header type element."))

(defclass header (header-type) ()
  (:documentation
   "Represent a header element."))

(defclass subheader (header-type) ()
  (:documentation
   "Represent a subheader element."))

(defclass subsubheader (header-type) ()
  (:documentation
   "Represent a subsubheader element."))


;; ----- text -----

(defclass text-type (element)
  ((text-elements :initarg :text-elements
		  :type list))
  (:documentation
   "Represents a text type element."))

(defclass text (element)
  (:documentation
   "Represents a text element."))


;; ----- text enrichment -----

(defclass text-enrichment (text-type)
  (:documentation
   "Represent a text enrichment element."))

(defclass bold (text-enrichment)
  (:documentation
   "Represent a bold element."))

(defclass italic (text-enrichment)
  (:documentation
   "Represent a italic element."))

(defclass bold-italic (text-enrichment)
  (:documentation
   "Represent a bold and italic element."))

(defclass code-inline (text-enrichment)
  (:documentation
   "Represent a code inline element."))


;; ----- text reference -----

(defclass header-ref (tagged-element)
  ((header-tags :type tag-table))
  (:documentation
   "Represent a header reference element."))


(defclass symbol-ref (tagged-element)
  ((symbol-tags :type tag-table))
  (:documentation
   "Represent a symbol reference element."))


(defclass function-ref (tagged-element)
  ((function-tags :type tag-table))
  (:documentation
   "Represent a function reference element."))


(defclass type-ref (tagged-element)
  ((type-tags :type tag-table))
  (:documentation
   "Represent a type reference element."))


;; ----- web link -----

(defclass web-link (element)
  ((text :initarg :text
	 :type string)
   (address :initarg :address
	    :type string))
  (:documentation
   "Represents a web-link element."))


;; ----- image -----

(defclass image (element)
  ((path :initarg :path
	 :type pathname))
  (:documentation
   "Represent an image element."))


;; ----- table -----

(defclass cell (text-type)
  (:documentation
   "Represent a cell table element."))

(defclass table (element)
  ((rows :initarg :rows
	 :type list))
  (:documentation
   "Represent a table element. Each row is a list of text elements."))


;; ----- itemize -----

(defclass item (text-type)
  (:documentation
   "Represents an item element."))

(defclass itemize-type (element)
  ((elements :initarg :elements
	     :type list))
  (:documentation
   "Represents an itemize element."))

(defclass itemize (itemize-type)
  (:documentation
   "Represent an itemize element."))

(defclass enumerate (itemize-type)
  (:documentation
   "Represent an enumerate element."))


;; ----- code -----

(defclass code (element)
  ((expr :initarg :expr)
   (hide-symbol :initform '#:hide
		:allocation :class
		:type symbol)
   (comment-symbol :initform '#:comment-symbol
		   :allocation :class
		   :type symbol))
  (:documentation
   "Represent a code element."))

(defclass tagged-code (code tagged-element)
  (:documentation
   "Represent a tagged code element."))


(defclass code-block-type (element)
  ((code-type :initarg :code-type
	      :type string))
  (:documentation
   "Represent a code block type element."))

(defclass code-block (code-block-type)
  ((code-elements :initarg :code-elements
		  :type list))
  (:documentation
   "Represent a code block element."))

(defclass code-ref (tagged-element)
  ((code-tags :type tag-table))
  (:documentation
   "Represent a code reference element."))

(defclass verbatim-code-block (code-block-type)
  ((code-text :initarg :code-text
	      :type string))
  (:documentation
   "Represent a verbatim code block element."))

(defclass code-example (element)
  ((code-elements :initarg :code-elements
		  :type list)
   (output :initarg :output
	   :type string)
   (result :initarg
	   :type list))
  (:documentation
   "Represent a code example element."))


;; ----- definition -----

(defclass definition (element)
  ((expr :initarg :expr))
  (:documentation
   "Represent a definition element."))

(defclass tagged-definition (definition tagged-element)
  (:documentation
   "Represent a tagged definition element."))

(defclass symbol-definition (tagged-definition) ()
  (:documentation
   "Represent a symbol tag definition element."))

(defclass function-definition (tagged-definition) ()
  (:documentation
   "Represent a function tag definition element."))

(defclass type-definition (tagged-definition) ()
  (:documentation
   "Represent a type tag definition element."))

(defmacro define-definition-class (name super docstring)
  `(defclass ,name (,super)
     (:documentation
      ,docstring)))

(define-definition-class defclass                  type-definition     "Represent a defclass element.")
(define-definition-class defconstant               symbol-definition   "Represent a defconstant element.")
(define-definition-class defgeneric                function-definition "Represent a defgeneric element.")
(define-definition-class define-compiler-macro     defintion           "Represent a define-compiler-macro element.")
(define-definition-class define-condition          type-definition     "Represent a define-condition element.")
(define-definition-class define-method-combination definition          "Represent a define-method-combination element.")
(define-definition-class define-modify-macro       function-definition "Represent a define-modify-macro element.")
(define-definition-class define-setf-expander      definition          "Represent a define-setf-expander element.")
(define-definition-class define-symbol-macro       symbol-definition   "Represent a define-symbol-macro element.")
(define-definition-class defmacro                  function-definition "Represent a defmacro element.")
(define-definition-class defmethod                 definition          "Represent a defmethod element.")
(define-definition-class defpackage                definition          "Represent a defpackage element.")
(define-definition-class defparameter              symbol-definition   "Represent a defparameter element.")
(define-definition-class defsetf                   definition          "Represent a defsetf element.")
(define-definition-class defstruct                 type-definition     "Represent a defstruct element.")
(define-definition-class deftype                   type-definition     "Represent a deftype element.")
(define-definition-class defun                     function-definition "Represent a defun element.")
(define-definition-class defvar                    symbol-definition   "Represent a defvar element.")