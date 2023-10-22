
(defpackage #:adp
  (:use #:cl)
  (:export #:*adp*
           #:add-element
           #:scribble
           #:file-component
           #:file-elements
           #:define-adp-operation
           #:define-adp-file
           #:pre-process-system
           #:post-process-system
           #:pre-process-file
           #:post-process-file
           #:export-content))
