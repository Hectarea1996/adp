
(in-package :adp)


(code-tag (code-user-guide-header)
  (header "The ADP User Guide" user-guide-header))

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

(text "As I said, I'm using headers in this guide. This is a header: " (header-ref user-guide-header) ". This is a subheader: " (header-ref setting-up-subheader) ". And this is a subsubheader: " (header-ref headers-subsubheader) ". Note that I can make a reference to a header. I can achieve this with header-tags. We will see this later in " (header-ref tags-subheader) ".")


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

(text "Note that spaces are placed out of enrichment functions (after " (code-inline "italic") " and " (code-inline "web-link") " calls). Also, you cannot nest calls of " (function-ref bold) ", " (function-ref italic) ", " (function-ref bold-italic) " and " (function-ref web-link) ". For example, if you try this:")

(code-block ()
  (text (bold (italic "This should be bold-italic."))))

(text "an error will be raised.")


(subsubheader "Images")

(text "You can add images with the macro " (function-ref image) ". For example, an image is located at " (code-inline "guides/images/") ". If I evaluate the next expression:")

(code-block (image-example)
  image-example)

(text "I get this:")

(code-tag (image-example)
  (image "Lisp logo" #P"guides/images/Lisp_logo.svg"))

(text "The first argument is the alternative text of the image. If for some reason the image cannot be loaded in some web page, the alternative text is used instead. The second argument is the pathname of the image, relative to the system's root directory.")


(subsubheader "Code blocks")

(text "A good Lisp tutorial must include Lisp code examples. ADP defines two macros to print code blocks: " (function-ref code-block) " and " (function-ref code-example) ". The main difference is that the former does not evaluate the code to be printed. So, for example:")

(code-block ()
  (code-block ()
    (this is not (valid code))
    (but it (is (ok)))))

(text "And you will see:")

(code-block ()
  (this is not (valid code))
  (but it (is (ok))))

(text "Note that " (code-inline 'nil) " is printed after " (code-inline "code-block") ". This is because we can pass some symbols to " (function-ref code-block) " in order to change a bit how it works. But, we will see that in later sections.")

(text "On the other hand, " (function-ref code-example) " do evaluate the code. And what is more, it prints the standard output as well as the returned values. For example:")

(code-block (code-example-example)
  code-example-example)

(text "And you will see:")

(code-tag (code-example-example)
  (code-example
    (loop for i from 0 below 10
	  do (print i)
	  finally (return (values "Hello" "world")))))

(text "Both with " (function-ref code-block) " and with " (function-ref code-example) " you can write multiple expressions.")


(subheader "Generating the documentation")

(text "There are still some useful macros that I didn't explain yet. However, I think it is now a good time to learn how to actually generate the documentation. You may have multiple files where the macros expalined above are used. But, where the documentation will be printed in? Which files will be generated?")

(text "First, we need to understand how ADP works. When you load a project and a file contains calls to some of the above macros, ADP will store information from these macros (functions or symbols defined, tables, lists, text, etc). At every moment we can decide to associate the information gathered so far with a file using the macro " (function-ref write-in-file) ". A good way to use ADP is calling " (function-ref write-in-file) " at the end of every file of code from your project. Doing that will create as many documentation files as code files your project has. Let's see an example. Imagine we have the following code in a file (it doesn't matter where it is located or even its name.)")

(code-block ()
  (header "My API")
  (defun foo ()
    "This function does a lot of things"
    (code-hide ()))
  (defun bar ()
    "This function also does a lot of things")
  (defparameter *global-param* (code-hide ()) "This parameter is awesome"))

(text "Now, we want to create a file where to print this information in. In order to do that, we must use " (function-ref write-in-file) ".")

(code-block ()
  (header "My API")
  (defun foo ()
    "This function does a lot of things"
    (code-hide ()))
  (defun bar ()
    "This function also does a lot of things")
  (defparameter *global-param* (code-hide ()) "This paramter is awesome")
  (write-in-file #P"docs/my-api"))

(text "This macro receives only the pathname to the file where to print in the documentation. The pathname must be relative to the system's root directory. Also note that I didn't use any extension in the pathname. That's because ADP let you choose between different styles to generate the documentation and each style will create their own files. After using " (function-ref write-in-file) ", the header, the two functions and the parameter are associated with the file that will be located at " (code-inline "docs/my-api") ". If the pathname was already used in another call to " (function-ref write-in-file) ", the new content will be appended to the information already gathered.")

(text "When all your documentation is associated with a file, it is time to generate the files and print the documentation. The function that must be used now is " (function-ref load-documentation-system) ". As the name suggests, you are going to load your system. In fact, it will load you system with the documentation generation enabled so, while forms are evaluated the documentation is gathered and also is associated with the pertinent files. When the system is completely loaded, the file generation and documentation printing begins. For example, if your system is named " (code-inline :my-system) ", then you can eval this expression in the REPL:")

(code-block ()
  (load-documentation-system :my-system :github-md))

(text "The second argument is the desired style. In this case the used style is " (code-inline :github-md) ". This style generates " (code-inline "md") " files to be used in the GitHub platform.")

(text "And that's all! The documentation is ready to be read.")


(subheader "Cross references" tags-subheader)

(text "ADP supports cross references with tags. A tag is a just a symbol with some information associated. There are five types of tags: header-tags, function-tags, symbol-tags, type-tags and code-tags.")


(subsubheader "Header-tags")

(text "A header-tag is a symbol with a header associated. We have already seen how to add a header to the documentation. But I didn't say that the macros " (function-ref header) ", " (function-ref subheader) " and " (function-ref subsubheader) " receives a second optional argument. As you can imagine, this second argument must be a symbol that will be converted to a header tag. For example, the first header of this file is created with this expression:")

(code-block (code-user-guide-header)
  code-user-guide-header)

(text "Now the symbol " (code-inline "user-guide-header") " is a header-tag. We can make a reference to that header with the macro " (function-ref header-ref) ". For example, if I write this:")

(code-block (header-ref-example)
  header-ref-example)

(text "Then you will see this:")

(code-tag (header-ref-example)
  (text "Go to the top: " (header-ref user-guide-header)))

(text "Note that the macro is used inside a " (function-ref text) " form. Same as with " (function-ref bold) " or " (function-ref italic) " " (function-ref header-ref) " only can be used inside " (function-ref text) ", " (function-ref table) " and " (function-ref itemize) ".")


(subsubheader "Function-tags, symbol-tags and type-tags" function-tags-subsubheader)

(text "These tags are symbols associated with a function, a variable or a type. More specifically, the macros used to define things like " (function-ref defun) ", " (function-ref defparameter) " or " (function-ref defstruct) " can create automatically a function-tag, a symbol-tag or a type-tag respectively. The tag created is the symbol of the name of the function, variable or type defined respectively. ADP defines three types of tags because the same symbol can refer to a function, a variable and a type simultaneously. The next list shows what type of tags are defined by which macros:")

(itemize (:item "Function-tags:")
	 (:itemize (:item (function-ref defgeneric))
		   (:item (function-ref define-modify-macro))
		   (:item (function-ref defmacro))
		   (:item (function-ref defun)))
	 (:item "Symbol-tags:")
	 (:itemize (:item (function-ref defconstant))
		   (:item (function-ref define-symbol-macro))
		   (:item (function-ref defparameter))
		   (:item (function-ref defvar)))
	 (:item "Type-tags:")
	 (:itemize (:item (function-ref defclass))
		   (:item (function-ref define-condition))
		   (:item (function-ref defstruct))
		   (:item (function-ref deftype))))

(text "Same as with header-tags, we can make reference to functions, variables and types with " (function-ref function-ref) ", " (function-ref symbol-ref) " and " (function-ref type-ref) ". For example, to make a reference to an ADP macro:")

(code-block (function-tag-example)
  function-tag-example)

(text "You will see this:")

(code-tag (function-tag-example)
  (text "A reference to an ADP macro: " (function-ref header)))

(text "Note again that " (function-ref function-ref) " only can be used inside " (function-ref text) ", " (function-ref table) " or " (function-ref itemize) ". The same goes to " (function-ref symbol-ref) " and " (function-ref type-ref) ".")

(text "A cool thing about cross references is that you can make a reference to something that is not currently defined but will be. For example, a variable will be defined at the end of this file but we can make a reference now. I'm writing the next expression:")

(code-block (symbol-ref-example)
  symbol-ref-example)

(code-tag (symbol-ref-example)
  (text "In the future, we will define the symbol " (symbol-ref a-parameter-defined-at-the-end-of-the-file) "."))

(text "Finally, an example using a type-tag:")

(code-block (type-ref-example)
  type-ref-example)

(code-tag (type-ref-example)
  (text "Using a type tag: " (type-ref also-a-type?)))


(subsubheader "Code-tags")

(text "Code-tags work differently to those we have just seen above. Code-tags are used inside the " (function-ref code-block) " macro. Imagine that you are making a tutorial. You are explaining how some piece of code works and you test that code in a different file to make sure your tutorial is well done. But one day, you decide to change your code. Now the tutorial needs to be changed too. To avoid writing your code twice you can use code-tags. Suppose that your code looks like this:")

(code-block ()
  (defun sum-list (int-list)
    (loop for num in int-list
	  sum num))
  (sum-list '(1 2 3)))

(text "And you write in your tutorial that the function " (code-inline "sum-list") " can be used the way you tested before:")

(code-block ()
  (text "The sum-list function can be used like this:")
  (code-block ()
    (sum-list '(1 2 3))))

(text "But now you decide to use vectors rather than lists. You didn't use code-tags so you must change your code in two different places. Let's create now a code-tag using the macro " (function-ref code-tag) ". Unfortunately, the macro " (function-ref code-tag) " cannot be printed inside code-block. So, I will use " (code-inline "code-lag") " instead:")

(code-block ()
  (defun sum-list (int-list)
    (loop for num in int-list
	  sum num))
  (code-lag (sum-list-example)
    (sum-list '(1 2 3))))

(text "A code-tag named " (code-inline "sum-list-example") " is created and you can now use it in the tutorial:")

(code-block ()
  (text "The sum-list function can be used like this:")
  (code-block (sum-list-example)
    sum-list-example))

(text "First we indicate in the list after " (code-inline "code-block") " that we will use the code tag named " (code-inline "sum-list-example") ". Then, we use it. Now, each time you change the call to " (code-inline "sum-list") " in your test code the tutorial will be automatically changed. You can specify as many tags as you want in both " (function-ref code-tag) " and " (function-ref code-block) " macros.")

(text "Let's see a live example. Do you remember the symbol " (symbol-ref a-parameter-defined-at-the-end-of-the-file) " and the type " (type-ref also-a-type?) ". In the source file I have written this:")

(code-block ()
  (code-lag (end-parameter-code)
    (defparameter a-parameter-defined-at-the-end-of-the-file t))

  (code-lag (end-type-code)
    (deftype also-a-type? ()
      nil)))

(text "So, if I write this:")

(code-block (code-block-with-tag-example)
  code-block-with-tag-example)

(text "You will see this:")

(code-tag (code-block-with-tag-example)
  (code-block (end-parameter-code end-type-code)
    end-type-code
    (some-code-in (the-middle))
    end-parameter-code))


(subsubheader "Hiding your code")

(text "When explaining some piece of code you should focus on the important parts. Or, equivalently, you should hide the irrelevant ones. You can hide parts of the code using the form " (code-inline "code-hide") ". This form is neither a function nor a macro. It is just a form recognized by " (function-ref code-block) ", " (function-ref code-example) " and " (function-ref code-tag) ". I can't use a code block using " (code-inline "code-hide") " because it will be hidden. So, I'm using " (code-inline "code-kide") ". For example, if I write this:")

(code-block ()
  (code-block ()
    (let ((code-kide () (x 5) (y 6)))
      (doing some stuff)
      (doing more stuff))))

(text "You will see this:")

(code-block ()
  (let ((code-hide () (x 5) (y 6)))
    (doing some stuff)
    (doing more stuff)))

(text "You can use it in a code example too. Writing this:")

(code-block ()
  (code-example
    (loop for i from 0 below (code-kide () (length '(some-private-stuff your-pin your-password or-whatever)))
	  do (code-kide () (let ((irrelevant-code 5))
			     irrelevant-code))
	     (print "Here is the important code!!!")
	     (code-kide () (let ((more-irrelevant 6))
			     more-irrelevant)))))

(text "You will see this:")


(code-example
  (loop for i from 0 below (code-hide () (length '(some-private-stuff your-pin your-password or-whatever)))
	do (code-hide () (let ((irrelevant-code 5))
			   irrelevant-code))
	   (print "Here is the important code!!!")
	   (code-hide () (let ((more-irrelevant 6))
			   more-irrelevant))))


(text "The " (code-inline "code-hide") " form is similar to " (function-ref code-block) " or " (function-ref code-tag) ". It receives as first argument a list of tags but they only take effect inside the " (function-ref code-tag) " macro. Imagine you have the following piece of code in your project:")

(code-block (large-function-example)
  large-function-example)

(code-tag (x-tag y-tag z-tag large-function-example)
  (cl:defun some-large-function (x y z)
    (let ((code-hide (y-tag z-tag) (post-x (1+ x)))
	  (code-hide (x-tag z-tag) (post-y (1+ y)))
	  (code-hide (x-tag y-tag) (post-z (1+ z))))
      (loop for i from 0 below 10
	    collect (code-hide (y-tag z-tag) post-x into x-list)
	    collect (code-hide (x-tag z-tag) post-y into y-list)
	    collect (code-hide (x-tag) (cons post-y post-z) into yz-list)
	    finally (return (values (code-hide (y-tag z-tag) x-list)
				    (code-hide (x-tag z-tag) y-list)
				    (code-hide (x-tag) yz-list)))))))

(text "The example can be large and hard to read as well. This piece of code uses the variables " (code-inline 'x) ", " (code-inline 'y) " and " (code-inline 'z) " in different ways and you may want to explain how each one participates in the function. We can show different parts of the code depending of the tags we specify. In this case you can write this:")

(code-block ()
  (code-lag (x-tag y-tag z-tag)
	    (cl:defun some-large-function (x y z)
	      (let ((code-kide (y-tag z-tag) (post-x (1+ x)))
		    (code-kide (x-tag z-tag) (post-y (1+ y)))
		    (code-kide (x-tag y-tag) (post-z (1+ z))))
		(loop for i from 0 below 10
		      collect (code-kide (y-tag z-tag) post-x into x-list)
		      collect (code-kide (x-tag z-tag) post-y into y-list)
		      collect (code-kide (x-tag) (cons post-y post-z) into yz-list)
		      finally (return (values (code-kide (y-tag z-tag) x-list)
					      (code-kide (x-tag z-tag) y-list)
					      (code-kide (x-tag) yz-list))))))))

(text "Note that we are using three tags here. Also, we are indicating when a piece of code must be hidden using the corresponding tags. If I write this:")

(code-block (tag-x-example)
  tag-x-example)

(text "You will see this:")

(code-tag (tag-x-example)
  (code-block (x-tag)
    x-tag))

(text "Same occurs if I use " (code-inline 'y-tag) " and " (code-inline 'z-tag) ". If I write this:")

(code-block (tag-yz-example)
  tag-yz-example)

(text "You will see this:")

(code-tag (tag-yz-example)
  (code-block (y-tag)
    y-tag)
  (code-block (z-tag)
    z-tag))


(subheader "Tips and final comments")

(text "I hope this guide is useful. I usually see Common Lisp projects that looks awesome but they lack guides or even documentation. That's why I started to document all my projects and then I realized that I needed some tool to make it easier. I know that there are already other documentation generators, but none of them suits my needs. Luckily, Common Lisp makes doing this kind of tools relatively easy compared to other languages. Lastly, I want to give you some tips or ways to use ADP that I ended up doing myself.")

(itemize (:item (bold-italic "Use a different system for documentation generation") ": I recommend to use a different system to indicate all the files you need to load to generate the documentation. So, if you have a system named " (code-inline :my-system) " then create another system named " (code-inline :my-system/docs) ", for example. Although ADP will not execute anything unless you use the function " (function-ref load-documentation-system) ", I think this should make your projects cleaner. And, let's be honest, I'm still learning the language and I don't want to break other people's code. I did this for ADP, so you can see an example in the file " (code-inline "adp.asd") ".")
	 (:item (bold-italic "Handling error messages") ": I tried to make informative error messages but sometimes this cannot be possible. Or, at least, I can't do it better. The most common errors I have had when using ADP were undefined variable errors. Remember that " (function-ref code-inline) " works the same as " (function-ref text) ". You can't write " (code-inline "(code-inline name-of-function)") ", you must write this instead " (code-inline "(code-inline \"name-of-function\")") " or " (code-inline "(code-inline 'name-of-function)") ". Also, be careful when using " (function-ref function-ref) " or similars. If you don't write correctly the macro, some implementations will treat that call as a function call and will treat the argument as a variable. That's not a variable that ADP or you have defined and it is sure that it will raise an undefined variable error.")
	 (:item "That's all! Enjoy using ADP. I leave you with " (symbol-ref a-parameter-defined-at-the-end-of-the-file) " and " (type-ref also-a-type?) " again."))



(code-tag (end-parameter-code)
  (defparameter a-parameter-defined-at-the-end-of-the-file t))

(code-tag (end-type-code)
  (deftype also-a-type? ()
    nil))

(text "Go back to " (header-ref function-tags-subsubheader))

(write-in-file #P"docs/user-guide")
