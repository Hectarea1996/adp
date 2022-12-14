
(in-package :adp)

(in-file #P"docs/user-guide")

(code-tag (code-user-guide-header)
  (header "The ADP User Guide" user-guide-header))

(text "Welcome to the ADP User Guide! Here you will learn how to add documentation to your projects using ADP. ADP can be divided in two groups of functions. The API functions and Guide function. However, despite that distinction all the functions can be mixed to generate your documentation.")

(text "I will try to do my best explaining how to use ADP. If this is not sufficient, note that every piece of documentation of ADP has been generated by itself. So you can see the source code and see how ADP has been used. For example, this file was generated using the source located at " (inline-code "guides/user-guide.lisp") ".")

(mini-table-of-contents)


(subheader "Setting up ADP" setting-up-subheader-segundo)

(text "After installing ADP, you must add it as a dependency in your project, like you have been doing for every project you want to use. For example, if you have a system like this:")

(code-block (defsystem-code)
  defsystem-code)

(code-tag (defsystem-code)
  (code-quote
   (asdf:defsystem :my-system
     (code-hide ())
     :depends-on (:uiop :alexandria)
     (code-hide ()))))

(text "You only need to add " (inline-code ":adp") " to the " (inline-code ":depends-on") " list.")

(code-block (adp-defsystem-code)
  adp-defsystem-code)

(code-tag (adp-defsystem-code)
  (code-quote
   (asdf:defsystem :my-system
     (code-hide ())
     :depends-on (:uiop :alexandria :adp)
     (code-hide ()))))

(text "Your system is now ready to use ADP.")


(subheader "Selecting a file to write in")

(text "Before start using the macros to write documentation, we need to select a file where to store it. We can do this using the " (function-ref in-file) " macro. You can always add this macro after " (cl-ref in-package) ".")

(code-block ()
  (in-package :my-pkg)

  (in-file #P"docs/my-file"))

(text "We need to pass a pathname to " (function-ref in-file) ". This pathname will be relative to the system's root directory. So, in this case a file named " (inline-code "my-file") " will be created inside the " (inline-code "docs") " directory in your system's root directory. Besides, note that the pathname does not include an extension. This is because later you can select between different styles and each style will generate different types of files. After writing this line of code we can start to use the rest of the macros.")

(subheader "Functions to generate the API")

(text "I'm sure your code defines a lot of things like functions, macros and symbols. In order to do that you have had to use define macros like " (cl-ref defun) ", " (cl-ref defmacro) " or " (cl-ref defparameter) ". Print some documentation of this definitions is very easy with ADP. For example, consider this function definition:")

(code-block ()
  (cl:defun foo (a b c)
    "Multiply a by the sum of b and c."
    (* a (+ b c))))

(text "If you want to generate documentation of this definition you only need to use the macro " (function-ref defun) " instead of " (cl-ref defun) ".")

(code-block (defun-example)
  defun-example)

(text "And you will see something like this:")

(code-tag (defun-example)
  (defun foo (a b c)
    "Multiply a by the sum of b and c."
    (* a (+ b c))))

(text "That's all! Actually, note that if you load your project as always after changing some defuns, you will see that nothing happens. Your system is loaded normally and nothing changes. This is because the documentation generation is disabled by default. So, even if you add ADP code, the original code remains the same.")

(text "Same as with " (function-ref defun) ", every macro that defines something is redefined to print documentation. You can see every macro here: " (header-ref api-subheader))


(subheader "Functions to generate guides.")

(text "The other group of functions are intended to generate guides and tutorials (like this one). But you can use them wherever you want and however you want. Even you can use them together with the API functions above.")


(subsubheader "Headers" headers-subsubheader)

(text "You can add headers in your documentation. In other words, they work as titles or subtitles. You can this way organize your guide with different sections (like I do in this guide). The macros that add headers are " (function-ref header) ", " (function-ref subheader) " and " (function-ref subsubheader) ". They need a string as the first argument. For example, if I write this:")

(code-block (headers-example)
  headers-example)

(text "You will see this:")

(code-tag (headers-example)
  (header "This is a header")
  (subheader "This is a subheader")
  (subsubheader "This is a subsubheader"))


(subsubheader "Text")

(text "When you want to add text you must use the macro " (function-ref text) ". It receives a variable number of arguments. Each argument is evaluated at run-time and its result is " (cl-ref princ) "-ed. Then, all the content that has been " (cl-ref princ) "-ed is concatenated into a single string and finally it is printed in the documentation file. For example:")

(code-block (text-example)
  text-example)

(text "If I use that right now:")

(code-tag (text-example)
  (text "This is the text macro. The result of 3+4 is " (+ 3 4) ". As we will see later you can enrich the text with " (bold "bold words") ", " (italic "italic words") ", " (emphasis "emphasis words") " and more."))


(subsubheader "Tables")

(text "You can add tables using the macros " (function-ref table) " and " (function-ref cell) ". The best way to see how to use it is an example. Imagine we want to show some data stored in some variables.")


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
  (table ((cell "Age") (cell "Name") (cell "Salary"))
	 ((cell (get-age peter-info)) (cell (get-name peter-info)) (cell (get-salary peter-info) "???"))
	 ((cell (get-age maria-info)) (cell (get-name maria-info)) (cell (get-salary maria-info) "???"))
	 ((cell (get-age laura-info)) (cell (get-name laura-info)) (cell (get-salary laura-info) "???"))))

(text "Note that in the " (italic "Salary") " column we used multiple values in each cell. Each call to "(function-ref cell)" can accept multiple values and they are treated as if they are in the " (function-ref text) " macro. In other words, each element in a cell is " (cl-ref princ) "-ed and the results are concatenated.")


(subsubheader "Lists")

(text "You can add lists with " (function-ref itemize) " or " (function-ref enumerate) ". For example:")

(code-block (list-example)
  list-example)

(text "You will see this:")

(code-tag (list-example)
  (itemize (item "Vegetables:")
	   (enumerate (item 3 " peppers:")
		      (itemize (item 1 " green pepper")
			       (item (- 3 1) " red pepper"))
		      (item 0.25 "Kg of carrots"))
	   (item "Fruits:")
	   (enumerate (item 0.5 "Kg of apples")
		      (item 6 " oranges"))))

(text "Note that each item inside " (function-ref itemize) " is a list starting with " (function-ref item) ", " (function-ref itemize) " or " (function-ref itemize) ". When you use " (function-ref item) " every object will be " (cl-ref princ) "-ed and then concatenated. In other words, it works the same as " (function-ref text) " or " (function-ref cell) ". On the other hand, when using " (function-ref itemize) " or " (function-ref enumerate) " you are indicating that you want a sublist of items.")


(subsubheader "Text enrichment")

(text "Inside a " (function-ref text) " form, a " (function-ref cell) " from a " (function-ref table) " form and a " (function-ref item) " from a " (function-ref itemize) " or " (function-ref enumerate) " form, we can enrich the text with the macros " (function-ref bold) ", " (function-ref italic) ", " (function-ref emphasis) " and " (function-ref web-link) ". For example:")

(code-block (rich-text-example)
  rich-text-example)

(text "You will see this:")

(code-tag (rich-text-example)
  (text "As " (bold "Andrew") " said: " (italic "You only need " (+ 1 2 3)) " " (web-link "coins" "https://en.wikipedia.org/wiki/Coin") " " (italic "to enter in") " " (emphasis "The Giant Red Tree.")))

(text "You cannot nest calls of " (function-ref bold) ", " (function-ref italic) ", " (function-ref emphasis) " and " (function-ref web-link) ". For example, if you try this:")

(code-block ()
  (text (bold (italic "This should be emphasis."))))

(text "an error will be raised.")


(subsubheader "Images")

(text "You can add images with the macro " (function-ref image) ". For example, an image is located at " (inline-code "guides/images/") ". If I evaluate the next expression:")

(code-block (image-example)
  image-example)

(text "I get this:")

(code-tag (image-example)
  (image "Lisp logo" #P"guides/images/Lisp_logo.svg"))

(text "The first argument is the alternative text of the image. If for some reason the image cannot be loaded in some web page, the alternative text is used instead. The second argument is the pathname of the image, relative to the system's root directory.")


(subsubheader "Code blocks")

(text "A good Lisp tutorial must include Lisp code examples. ADP defines some macros to print code blocks: " (function-ref code-block) ", " (function-ref verbatim-code-block) " and " (function-ref code-example) ". The first macro does not evaluate the code. So, for example if you write this:")

(code-block ()
  (code-block ()
    (this is not (valid code))
    (but it (is (ok)))))

(text "You will see:")

(code-block ()
  (this is not (valid code))
  (but it (is (ok))))

(text "Note that " (inline-code 'nil) " or, equivalently, " (inline-code "()") " is used after " (inline-code "code-block") ". This is because we can pass some symbols to " (function-ref code-block) " in order to change a bit how it works. But, we will see that in later sections.")

(text "The macro " (function-ref verbatim-code-block) " allows you to write non-Lisp code. It must receive two strings denoting the programming language to be used and the code itself. For example, writing this:")

(code-block (verbatim-example)
  verbatim-example)

(text "You will see this:")

(code-tag (verbatim-example)
  (verbatim-code-block "C++"
      "int main (){

  // We initialize some vars
  int n = 5;
  int m = 10;
 
  // We print hello world
  std::count << \"Hello world\" << std::endl;

  return 0;

}"))

(text "Lastly, " (function-ref code-example) " evaluate the Lisp code you write on it. And what is more, it prints the standard output as well as the returned values. For example, writing this:")

(code-block (code-example-example)
  code-example-example)

(text "You will see:")

(code-tag (code-example-example)
  (code-example
    (loop for i from 0 below 10
	  do (print i))
    (values "Hello" "world")))


(subheader "Generating the documentation")

(text "There are still some useful macros that I didn't explain yet. However, I think it is now a good time to learn how to actually generate the documentation. You may have multiple files where the macros explained above are used. Also, all the information is associated with a file because you have used the macro " (function-ref in-file) " whereever you have needed.")

(text "Now it is time to generate the files and print the documentation. The function that must be used now is " (function-ref load-system) ". As the name suggests, you are going to load your system. In fact, it will load your system with the documentation generation enabled so, while forms are evaluated the documentation is gathered. When the system is completely loaded, the file generation and documentation printing begins. For example, if your system is named " (inline-code :my-system) ", then you can eval this expression in the REPL:")

(code-block ()
  (load-system :my-system :github-md))

(text "The second argument is the desired style. In this case the used style is " (inline-code :github-md) ". This style generates " (inline-code "md") " files to be used in the GitHub platform.")

(text "I did this as well to generate the ADP documentation. If you take a look at the file " (inline-code "adp.asd") " you will see a system named " (inline-code "adp/doc") ". That system loads every file that is needed to generate all the ADP documentation. And I did that using this expression:")

(code-block ()
  (adp:load-system :adp/doc :github-md))

(text "And that's all! The documentation is ready to be read.")


(subheader "Cross references" tags-subheader)

(text "ADP supports cross references with tags. A tag is a just a symbol with some information associated. There are six types of tags: header-tags, function-tags, symbol-tags, type-tags, file-tags and code-tags.")


(subsubheader "Header-tags")

(text "A header-tag is a symbol with a header associated. We have already seen how to add a header to the documentation. But I didn't say that the macros " (function-ref header) ", " (function-ref subheader) " and " (function-ref subsubheader) " receives a second optional argument. As you can imagine, this second argument must be a symbol that will be converted to a header tag. For example, the first header of this file is created with this expression:")

(code-block (code-user-guide-header)
  code-user-guide-header)

(text "Now the symbol " (inline-code "user-guide-header") " is a header-tag. We can make a reference to that header with the macro " (function-ref header-ref) ". For example, if I write this:")

(code-block (header-ref-example)
  header-ref-example)

(text "Then you will see this:")

(code-tag (header-ref-example)
  (text "Go to the top: " (header-ref user-guide-header)))

(text "Note that the macro is used inside a " (function-ref text) " form. Same as with " (function-ref bold) " or " (function-ref italic) ", " (function-ref header-ref) " only can be used inside " (function-ref text) ", " (function-ref table) " or " (function-ref itemize) ".")


(subsubheader "Function-tags, symbol-tags and type-tags" function-tags-subsubheader)

(text "These tags are symbols associated with a function, a variable or a type respectively. More specifically, the macros used to define things like " (function-ref defun) ", " (function-ref defparameter) " or " (function-ref defstruct) " can create automatically a function-tag, a symbol-tag or a type-tag respectively. The tag created is the symbol of the name of the function, variable or type defined respectively. ADP defines three types of tags because the same symbol can refer to a function, a variable and a type simultaneously. The next list shows what type of tags are defined by which macros:")

(itemize (item "Function-tags:")
	 (itemize (item (function-ref defgeneric))
		   (item (function-ref define-modify-macro))
		   (item (function-ref defmacro))
		   (item (function-ref defun)))
	 (item "Symbol-tags:")
	 (itemize (item (function-ref defconstant))
		   (item (function-ref define-symbol-macro))
		   (item (function-ref defparameter))
		   (item (function-ref defvar)))
	 (item "Type-tags:")
	 (itemize (item (function-ref defclass))
		   (item (function-ref define-condition))
		   (item (function-ref defstruct))
		   (item (function-ref deftype))))

(text "Same as with header-tags, we can make reference to functions, variables and types with " (function-ref function-ref) ", " (function-ref symbol-ref) " and " (function-ref type-ref) ". For example, to make a reference to an ADP macro:")

(code-block (function-tag-example)
  function-tag-example)

(text "You will see this:")

(code-tag (function-tag-example)
  (text "A reference to an ADP macro: " (function-ref header)))

(text "Note again that " (function-ref function-ref) " only can be used inside " (function-ref text) ", " (function-ref cell) " or " (function-ref item) ". The same goes to " (function-ref symbol-ref) " and " (function-ref type-ref) ".")

(text "A cool thing about cross references is that you can make a reference to something that is not currently defined but will be. For example, a variable will be defined at the end of this file but we can make a reference now. I'm writing the next expression:")

(code-block (symbol-ref-example)
  symbol-ref-example)

(code-tag (symbol-ref-example)
  (text "In the future, we will define the symbol " (symbol-ref a-parameter-defined-at-the-end-of-the-file) "."))

(text "Lastly, an example using a type-tag:")

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

(text "And you write in your tutorial that the function " (inline-code "sum-list") " can be used the way you tested before:")

(code-block ()
  (text "The sum-list function can be used like this:")
  (code-block ()
    (sum-list '(1 2 3))))

(text "But now you decide to use vectors rather than lists. You didn't use code-tags so you must change your code in two different places. Let's create now a code-tag using the macro " (function-ref code-tag) ":")

(code-block ()
  (defun sum-list (int-list)
    (loop for num in int-list
	  sum num))
  (code-tag (sum-list-example)
    (sum-list '(1 2 3))))

(text "A code-tag named " (inline-code "sum-list-example") " is created and you can now use it in the tutorial:")

(code-block ()
  (text "The sum-list function can be used like this:")
  (code-block (sum-list-example)
    sum-list-example))

(text "First we indicate in the list after " (inline-code "code-block") " that we will use the code tag named " (inline-code "sum-list-example") ". Then, we use it. Now, each time you change the call to " (inline-code "sum-list") " in your test code the tutorial will be automatically changed. You can specify as many tags as you want in both " (function-ref code-tag) " and " (function-ref code-block) " macros.")

(text "Let's see a live example. Do you remember the symbol " (symbol-ref a-parameter-defined-at-the-end-of-the-file) " and the type " (type-ref also-a-type?) "?. In the source file I have written this:")

(code-block ()
  (code-tag (end-parameter-code)
    (defparameter a-parameter-defined-at-the-end-of-the-file t))

  (code-tag (end-type-code)
    (deftype also-a-type? ()
      'vector)))

(text "So, if I write this:")

(code-block (code-block-with-tag-example)
  code-block-with-tag-example)

(text "You will see this:")

(code-tag (code-block-with-tag-example)
  (code-block (end-parameter-code end-type-code)
    end-type-code
    (some-code-in (the-middle))
    end-parameter-code))


(subsubheader "Quoting, commenting, hiding and removing your code")

(text "Code tags are great but " (function-ref code-tag) " will expand to the code you write. Sometimes you will want to create a tag to some piece of code but not to evaluate it. In that case we can quote the code using the form " (function-ref code-quote) ". For example, you can write this:")

(code-block ()
  (code-block (quoted-code)
    quoted-code)

  (code-tag (quoted-code)
    (code-quote
     (this is a (form i (dont want) to evaluate)))))

(text "And you will see this:")

(code-block (quoted-code)
  quoted-code)

(code-tag (quoted-code)
  (code-quote
   (this is a (form i (dont want) to evaluate))))

(text "Comments are ignored when Lisp is reading an expression, so you cannot place a regular comment inside a " (function-ref code-block) " form and expect to see it printed. If you want to print a comment you need to use the form " (function-ref code-comment) " inside " (function-ref code-tag) ". It receives a string and an expression. For example, if I write this:")

(code-block ()
  (code-block (commented-code)
    commented-code)
  
  (code-tag (commented-code)
    (code-quote
     (let ((x 5))
       (code-comment "We print the number 5"
		     (print x))))))

(text "you will see this:")

(code-block (commented-code)
  commented-code)

(code-tag (commented-code)
  (code-quote
   (let ((x 5))
     (code-comment "We print the number 5"
		   (print x)))))

(text "When explaining some piece of code you should focus on the important parts. Or, equivalently, you should hide the irrelevant ones. You can do that using the form " (function-ref code-hide) ". For example, if I write this:")

(code-block ()
  (code-block (hidden-code)
    hidden-code)

  (code-tag (hidden-code)
    (code-quote
     (let ((code-hide () (x 5) (y 6)))
       (doing stuff)
       (doing more stuff)))))

(text "You will see this:")

(code-block (hidden-code)
  hidden-code)

(code-tag (hidden-code)
  (code-quote
   (let ((code-hide () (x 5) (y 6)))
     (doing stuff)
     (doing more stuff))))


(text "Note that the hidden code is shown as three dots ('...'). This form receives as first argument a list of tags. If the list is empty, the hide of the code will take effect always, but you can specify when to hide or remove your code specifing a tag. Imagine you have the following piece of code in your project:")

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

(text "The example can be large and hard to read as well. This piece of code uses the variables " (inline-code 'x) ", " (inline-code 'y) " and " (inline-code 'z) " in different ways and you may want to explain how each one participates in the function. We can show different parts of the code depending of the tags we specify. In this case you can write this:")

(code-block ()
  (code-tag (x-tag y-tag z-tag)
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
				      (code-hide (x-tag) yz-list))))))))

(text "Note that we are using three tags here. Also, we are indicating when a piece of code must be hidden using the corresponding tags. If I write this:")

(code-block (tag-x-example)
  tag-x-example)

(text "You will see this:")

(code-tag (tag-x-example)
  (code-block (x-tag)
    x-tag))

(text "Same occurs if I use " (inline-code 'y-tag) " and " (inline-code 'z-tag) ". If I write this:")

(code-block (tag-yz-example)
  tag-yz-example)

(text "You will see this:")

(code-tag (tag-yz-example)
  (code-block (y-tag)
    y-tag)
  (code-block (z-tag)
    z-tag))

(text "You can also remove the code using the form " (function-ref code-remove) ". It works the same as " (function-ref code-hide) ". For example, you can create a making-function explanation:")

(code-block ()
  (code-block (making-function-1)
    making-function-1)

  (code-block (making-function-2)
    making-function-2)
  
  (code-tag (making-function-1 making-function-2)
    (cl:defun print-5-6 ()
      (code-hide (making-function-2)
		 (code-comment "We print 5")
		 (print 5))
      (code-remove (making-function-1)
		   (code-comment "And we print 6")
		   (print 6)))))

(text "And you will see this:")

(code-block (making-function-1)
  making-function-1)

(code-block (making-function-2)
  making-function-2)

(code-tag (making-function-1 making-function-2)
  (cl:defun print-5-6 ()
    (code-hide (making-function-2)
	       (code-comment "First we print 5"
			     (print 5)))
    (code-remove (making-function-1)
		 (code-comment "And then we print 6"
			       (print 6)))))


(subheader "Tips and final comments")

(text "I hope this guide is useful. I usually see Common Lisp projects that looks awesome but they lack guides or even documentation. That's why I started to document all my projects and then I realized that I needed some tool to make it easier. I know that there are already other documentation generators, but none of them suits my needs. Luckily, Common Lisp makes doing this kind of tools relatively easy compared to other languages. Lastly, I want to give you some tips or ways to use ADP that I ended up doing myself.")

(itemize (item (emphasis "Use a different system for documentation generation") ": I recommend to use a different system to indicate all the files you need to load to generate the documentation. So, if you have a system named " (inline-code :my-system) " then create another system named " (inline-code :my-system/docs) ". Although ADP will not execute anything unless you use the function " (function-ref load-system) ", I think this should make your projects cleaner. And, let's be honest, I'm still learning the language and I don't want to break other people's code. I did this separation for ADP, so you can see an example in the file " (inline-code "adp.asd") ".")
	 (item (emphasis "Handling error messages") ": I tried to make informative error messages but sometimes this cannot be possible. Or, at least, I can't do it better. The most common errors I have had when using ADP were undefined variable errors. Remember that " (function-ref inline-code) " works the same as " (function-ref text) ". You can't write " (inline-code "(inline-code name-of-function)") ", you must write this instead " (inline-code "(inline-code \"name-of-function\")") " or " (inline-code "(inline-code 'name-of-function)") ". Also, be careful when using " (function-ref function-ref) " or similars. If you don't write correctly the macro, some implementations will treat that call as a function call and will treat the argument as a variable. That's not a variable that ADP or you have defined and it is sure that it will raise an undefined variable error.")
	 (item (emphasis "Tags belong to a package!") ": Note that tags are actually symbols, and symbols belong to a package. If you define a tag and you want to make a reference to it from another package, remember to add the package extension to the symbol name. For example, suppose that you define the symbol-tag " (inline-code 'my-tag) " in the package " (inline-code :my-pkg) ". Then, in another package you must write " (inline-code "(symbol-ref my-pkg:my-tag)") ", or " (inline-code "(symbol-ref my-pkg::my-tag)") " if the symbol is not exported. And yes, you should export the tags you want to use from other packages.")
	 (item (emphasis "Read the API") ": Maybe reading " (header-ref user-api-header) " can make you understand better how some macros work (or not). At least, you may be interested in seeing the section " (header-ref additional-functions-subheader) " or " (header-ref macro-characters-subheader) ".")
	 (item (emphasis "That's all! Enjoy using ADP.") " I leave you with " (symbol-ref a-parameter-defined-at-the-end-of-the-file) " and " (type-ref also-a-type?) " again."))



(code-tag (end-parameter-code)
  (defparameter a-parameter-defined-at-the-end-of-the-file t))

(code-tag (end-type-code)
  (deftype also-a-type? ()
    'vector))

(text "Go back to " (header-ref function-tags-subsubheader))
