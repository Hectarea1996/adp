
(in-package :adp)


(header "The ADP User Guide" user-guide-header)

(text "Welcome to the ADP User Guide! Here you will learn how to add documentation to your projects using ADP. ADP can be divided in two groups of functions. The API functions and Guide function. However, despite that distinction all the functions can be mixed to generate your documentation.")

(text "I will try to do my best explaining how to use ADP. If this is not sufficient, note that every piece of documentation of ADP has been generated by itself. So you can see the source code and see how ADP has been used. For example, this file was generated using the source located at " (code-inline "guides/user-guide.lisp") ".")


(subheader "Setting up ADP" setting-up-subheader)

(text "After installing ADP, you must add it as a dependency in your project, like you have been doing for every project you want to use. For example, if you have a system like this:")

(code-block ()
  (asdf:defsystem :my-system
    (code-hide ())
    :depends-on (:uiop :alexandria)
    (code-hide ())))

(text "You only need to add " (code-inline ":adp") " to the " (code-inline "depends on") " list.")

(code-block ()
  (asdf:defsystem :my-system
    (code-hide ())
    :depends-on (:uiop :alexandria :adp)
    (code-hide ())))

(text "Your system is now ready to use ADP.")

(subheader "Functions to generate the API")

(text "I'm sure your code defines a lot of things like functions, macros and symbols. In order to do that you have had to use define macros like " (cl-ref defun) ", " (cl-ref defmacro) " or " (cl-ref defparameter) ". Print some documentation of this definitions is very easy with ADP. For example, consider this function definition:")

(code-block ()
  (cl:defun foo (a b c)
    "Multiply a by the sum of b and c."
    (* a (+ b c))))

(text "If you want to generate documentation of this definition you only need to use the macro " (function-ref defun) " instead of " (cl-ref defun) ".")

(code-block ()
  (defun foo (a b c)
    "Multiply a by the sum of b and c."
    (* a (+ b c))))

(text "That's all! Actually, note that if you load your project as always after changing some defuns, you will see that nothing happens. Your system is loaded normally and nothing changes. This is because the documentation generation is disabled by default. So, even you add ADP code your original code remains the same.")

(text "Same as with " (function-ref defun) ", every macro that defines something is redefined to print documentation. You can see every macro here: " (header-ref api-subheader))


(subheader "Functions to generate guides.")

(text "The other group of functions are intended to generate guides and tutorials (like this one). But you can use them wherever you want and however you want. Even you can use them together with the API functions above.")


(subsubheader "Headers" headers-subsubheader)

(text "You can add headers in your documentation. In other words, they work as titles or subtitles. You can this way organize your guide with different sections (like I do in this guide). The macros that add headers are " (function-ref header) ", " (function-ref subheader) " and " (function-ref subsubheader) ". They need a string as the first argument.")

(code-block ()
  (header "The title of my guide.")
  (subheader "A section of my guide.")
  (subsubheader "A subsection of my guide."))

(text "As I said, I'm using headers in this guide. This is a header: " (header-ref user-guide-header) ". This is a subheader: " (header-ref setting-up-subheader) ". And this is a subsubheader: " (header-ref headers-subsubheader) ". Note that I can make a reference to a header. I can achieve this with header-tags. We will see this later in " (header-ref tags-subsubheader) ".")


(subsubheader "Text")

(text "When you want to add text you must use the macro " (function-ref text) ". It receives a variable number of arguments. Each argument is evaluated at run-time and its result is " (cl-ref princ) "-ed. Then, all the content that has been " (cl-ref princ) "-ed is concatenated into a single string and finally it is printed in the documentation file. For example:")

(code-block ()
  (text "This is the text macro. The result of 3+4 is " (+ 3 4) ". As we will see later you can enrich the text with " (bold "bold words") ", " (italic "italic words") ", " (bold-italic "bold-italic words") " and more."))

(text "If I use that right now:")

(text "This is the text macro. The result of 3+4 is " (+ 3 4) ". As we will see later you can stylize the text with " (bold "bold words") ", " (italic "italic words") ", or " (bold-italic "bold-italic words") " among other styles.")


(subsubheader "Tables")

(text "You can add tables using the macro " (function-ref table) ". The best way to see how to use it is an example. Imagine we want to show some data stored in some variables.")


(code-tag (info-table)
  (cl:defparameter peter-info '(34 "Peter Garcia" 1435))
  (cl:defparameter maria-info '(27 "Maria Martinez" 1765))
  (cl:defparameter laura-info '(53 "Laura Beneyto" 1543))

  (cl:defun get-age (info)
    (first info))

  (cl:defun get-name (info)
    (second info))

  (cl:defun get-salary (info)
    (third info)))

(code-block (info-table)
  info-table)

(text "Now we can create a table like this:")

(code-block (table-example)
  table-example)

(text "And you will see this:")

(code-tag (table-example)
  (table ((:cell "Age") (:cell "Name") (:cell "Salary"))
	 ((:cell (get-age peter-info)) (:cell (get-name peter-info)) (:cell (get-salary peter-info) "€"))
	 ((:cell (get-age maria-info)) (:cell (get-name maria-info)) (:cell (get-salary maria-info) "€"))
	 ((:cell (get-age laura-info)) (:cell (get-name laura-info)) (:cell (get-salary laura-info) "€"))))

(text "Note that in the " (italic "Salary") " column we used multiple values in each cell. Each cell can accept multiple values and they are treated as if they are in the " (function-ref text) " macro. In other words, each element in a cell is " (cl-ref princ) "-ed and the results are concatenated.")


(subsubheader "Lists")

(text "You can add lists with " (function-ref itemize) ". For example:")

(code-block (list-example)
  list-example)

(text "You will see this:")

(code-tag (list-example)
  (itemize (:item "Vegetables:")
	   (:itemize (:item 3 " peppers:")
		     (:itemize (:item 1 " green pepper")
			       (:item (- 3 1) " red pepper"))
		     (:item 0.25 "Kg of carrots"))
	   (:item "Fruits:")
	   (:itemize (:item 0.5 "Kg of apples")
		     (:item 6 " oranges"))))

(text "Note that each item inside " (function-ref itemize) " is a list starting with " (code-inline :item) " or " (code-inline :itemize) ". When you use " (code-inline :item) " every object will be " (cl-ref princ) "-ed and then concatenated. In other words, it works the same as " (function-ref text) " or " (function-ref table) ". On the other hand, when using " (code-inline :itemize) " you are indicating that you want a sublist of items.")


(subsubheader "Text enrichment")

(text "Inside a " (function-ref text) " form, a " (code-inline :cell) " from a " (function-ref table) " form and a " (code-inline :item) " form a " (function-ref itemize) " form, we can enrich the text with the macros " (function-ref bold) ", " (function-ref italic) ", " (function-ref bold-italic) " and " (function-ref web-link) ". For example:")

(code-block (rich-text-example)
  rich-text-example)

(text "You will see this:")

(code-tag (rich-text-example)
  (text "As " (bold "Andrew") " said: " (italic "You only need " (+ 1 2 3)) " " (web-link "coins" "https://en.wikipedia.org/wiki/Coin") " " (italic "to enter in") " " (bold-italic "The Giant Red Tree.")))

(text "It is good to know that you cannot nest calls of " (function-ref bold) ", " (function-ref italic) ", " (function-ref bold-italic) " and " (function-ref web-link) ". For example, if you try this:")

(code-block ()
  (text (bold (italic "This should be bold-italic."))))

(text "an error will be raised.")

(text "")


(subsubheader "Tags and references" tags-subsubheader)


(write-in-file #P"docs/user-guide")
