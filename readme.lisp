
(in-package :adp)

(write-in-file #P"README")

(header "Add Documentation, Please")

(text "Welcome to ADP!")

(subheader "Introduction")

(text (italic "Add Documentation, Please") " is a library for literate programming and semi-automatic API generation. There are already good projects for literate programming like " (code-inline "Erudite") " so, why another project? Well, they work differently and " (code-inline "Erudite") " just doesn't adjust to my needs.")

(text (bold "ADP") " is simple but practical. To generate documentation you have to use different macros. For example, if you want a header, then you use the " (function-ref header) " macro. Or if you want a block of code you use " (function-ref code-block) ". The reason behind using macros to literate programming is that you can make your own macros using the ones exported by " (bold "ADP") ".")

(text "Generating the API documentation is also easy. Suppose that you have the following function definition:")

(code-block ()
  (cl:defun foo ()
    "A function that does nothing."
    (values)))

(text "ADP redefines the macro " (cl-ref defun) ". To generate the documentation for this function you just need to tell Common Lisp that the macro " (function-ref defun) " used is the one from the package " (code-inline "adp") ":")

(code-block ()
  (adp:defun foo ()
    "A function that does nothing"
    (values)))

(text "That's all! And the same occurs with every Common Lisp macro that defines something, like " (function-ref defpackage) " or " (function-ref define-method-combination) ".")

(text "You may be thinking that this will make your code slower because now your code is gathering information for printing documentation. But that is not the case. The documentation generation is controlled by a global variable. This way, when you load your system like always, ADP will do nothing. Literally (try to macroexpand some ADP macro). The generation is activated only when you load your system using the function " (function-ref load-documentation-system) ". Even you can create a different system for loading the files you need for documentation.")

(text "Finally, you can also choose between several styles. Each style creates different files. For example, the style " (code-inline :github-md) " generates " (code-inline "md") " files. In fact, the readme file you are reading right now has been generated by ADP, so if this is a markdown file you are seeing the " (code-inline :github-md) " style. Another style could generate " (code-inline "html") " files or " (code-inline "tex") " files.")


(subheader "Installation")

(text "ADP is available at Ultralisp. If you don't have it, add it to Quicklisp:")

(code-block ()
  (ql-dist:install-dist "http://dist.ultralisp.org/"
			:prompt nil))

(text "And finally, install ADP:")

(code-block ()
  (ql:quickload :adp))


(subheader "Documentation")

(itemize (:item "The ADP guide: " (header-ref user-guide-header))
	 (:item "The ADP api: " (header-ref user-api-header))
	 (:item "The Style-Maker guide: " (header-ref style-maker-guide-header))
	 (:item "The Style-Maker api: " (header-ref adppvt:style-maker-api-header))
	 (:item "Style-maker helper functions: " (header-ref adppvt:style-maker-helper-header)))


(subheader "Available styles")

(table ((:cell "Keyword name") (:cell "File type") (:cell "Extra dependencies") (:cell "Authors"))
       ((:cell ":github-md") (:cell (code-inline "md")) (:cell nil) (:cell (web-link "Héctor Galbis Sanchis" "https://github.com/Hectarea1996"))))
