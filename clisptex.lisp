;;;; clisptex.lisp

(in-package #:clisptex)

;; fundamentals
(defun tex-to-string (input)
  (cond
   ((null input) nil)
   ((listp input) (concatenate 'string (tex-to-string (car input)) (tex-to-string (cdr input))))
   ((not (null input)) (format nil "~(~A~)" input))))

(defun tex-eval (content)
  (cond
   ;;"base cases"
   ((null content) nil)
   ((numberp content) content)
   ((atom content) (tex-to-string content))
   ;;"special forms"
   ((and (listp content) (eq (car content) 'list)) (apply #'list
							  (mapcar #'tex-eval
								  (cdr content))))
   ((and (listp content) (eq (car content) 'group)) (apply #'concat-space
							   (mapcar #'tex-eval
								   (cdr content))))
   ;;"handle nested lists"
   ((listp content) (apply (car content) (remove nil (tex-expand (cdr content)))))))

(defun tex-expand (content-list)
    (when content-list (cons (tex-eval (car content-list)) (tex-expand (cdr content-list)))))

(defun tex-eval-list (content)
  (if (listp (car content))
      (extract-list-of-strings (mapcar #'tex-eval content))
    (tex-eval content)))

;; Examples
;; (tex-eval '(pd u x 2))

;; (tex-eval-list
;;  '(tex-eq-pause
;;    (list
;;     (group (dd (um i) t) (pd (pm) (x i)))
;;     1+1=2)))

;; (tex-eval '(pd (pm) (x i)))
;; (tex-eval '(pd (pm) (angular-velocity i)))

;; (tex-eval '(tex-cmd alpha))
;; (tex-eval-list '((tex-cmd alpha) (um j)))

;; (tex-eval '(tex-eq-pause (list (group (um i) + 1 = 2) 2+2=4)))

;; (tex-eval '(group (um i) (pm)))

;; (tex-eval '(make-index (um i) (tex-cmd alpha)))

;; (tex-eval '(pm))

;; (tex-eval
;;  '(tex-eq-pause (list 1+1=2 2+2=4)))

;; (tex-eval-list '((pm) (um i) (um (pm))))

;; (um 'i nil)

(defun list-if-not-already (input)
  "writes input into a lisp unless it is already a list"
  (if (listp input)
      input
    (list input)))

(defun extract-list-of-strings (string-list &optional pre post)
  "extract a list of strings and frame each item by pre and post"
  (cond ((not (null string-list))
	 (concatenate 'string
	  (tex-to-string pre)
	  (tex-to-string (car string-list))
	  (tex-to-string post)
	  (extract-list-of-strings (cdr string-list) pre post)))))

(defun concat-space (&rest input)
  (if (and input (car input))
      (concatenate 'string
       (tex-to-string (car input))
       " "
       (apply #'concat-space (cdr input)))
    ""))

(defun repeat-string (string count)
  (when (> count 0)
    (concat-space
     (tex-to-string string)
     (repeat-string string (- count 1)))))

(defun wrap (inner deliml delimr)
  (concatenate 'string (tex-to-string deliml)
	  (tex-to-string inner)
	  (tex-to-string delimr)))

(defun make-exponent (main-expression exponent)
  (concatenate 'string (tex-to-string main-expression) "^" (wrap-braces exponent)))

(defun make-index (main-expression &rest indices)
  (concatenate 'string (tex-to-string main-expression) "_" (wrap-braces (extract-list-of-strings indices))))

(defun tex-newline () (string #\Newline))

(defun tex-eq-newline () (concatenate 'string "\\" (tex-newline)))

(defun tex-cmd (cmd-name &optional mandatory-args optional-args)
  "create a tex command accepting single arguments or multiple arguments as a list"
  (let ((mandatory-arg-list
	 (list-if-not-already mandatory-args))
	(optional-arg-list
	 (list-if-not-already optional-args)))
    (concatenate 'string "\\"
	    (tex-to-string cmd-name)
	    (extract-list-of-strings (remove nil optional-arg-list) "[" "]")
	    (extract-list-of-strings (remove nil mandatory-arg-list) "{" "}"))))

(defun tex-inverse-cmd (cmd-name &optional mandatory-args optional-args)
  "create a tex command accepting single arguments or multiple arguments as a list"
  (let ((mandatory-arg-list
	 (list-if-not-already mandatory-args))
	(optional-arg-list
	 (list-if-not-already optional-args)))
    (concatenate 'string "\\"
	    (tex-to-string cmd-name)
	    (extract-list-of-strings mandatory-arg-list "{" "}")
	    (extract-list-of-strings optional-arg-list "[" "]"))))

(defun tex-env (env-name content &optional mandatory-args optional-args pre-cmd post-cmd)
  "create a tex environment accepting single arguments or multiple arguments as a list"
  (let ((mandatory-arg-list
	 (list-if-not-already mandatory-args))
	(optional-arg-list
	 (list-if-not-already optional-args)))
    (concatenate 'string      (tex-newline)
     (tex-inverse-cmd "begin" (cons env-name mandatory-arg-list) optional-arg-list)
     (unless (null pre-cmd)
       (tex-newline)
       pre-cmd)
     (tex-newline)
     (tex-to-string content)
     post-cmd
     (tex-newline)
     (tex-cmd "end" (list env-name)))))

(defun tex-symbol (symbol-name)
  (concatenate 'string " " (tex-cmd symbol-name) " "))

(defun tex-label (label)
  (unless (null label) (concatenate 'string (tex-cmd "label" label) (tex-newline))))

(defun wrap-parens (inner)
  (wrap inner
	(tex-cmd "left(")
	(tex-cmd "right)")))

(defun wrap-braces (inner)
  (wrap inner
	 "{"
	 "}"))

(defun wrap-brackets (inner)
  (wrap inner
	(tex-cmd "left[")
	(tex-cmd "right]")))

;; header
(defun tex-documentclass (class &rest options)
  (concatenate 'string (tex-cmd 'documentclass class options)
	  (tex-newline)))

(defun tex-usepackage (package &rest options)
  (concatenate 'string (tex-cmd 'usepackage package options)
	  (tex-newline)))

(defun tex-usepackages (package-list)
  (unless (null package-list)
    (concatenate 'string      (cond
      ((listp (car package-list)) (tex-usepackage (caar package-list) (cadar package-list)))
      (t (tex-usepackage (car package-list))))
     (tex-usepackages (cdr package-list)))))

(tex-usepackages
 (list
  'times
  'amssymb
  (list 'inputenc 'utf8)
  )
 )

(defun tex-input-file (file)
  (concatenate 'string (tex-cmd 'input file)
	  (tex-newline)))

(defun tex-document (content)
  (concatenate 'string    (tex-newline)
   (tex-env 'document content)))
;; sections
(defun generic-tex-structure (structure-type title content)
  (concatenate 'string    (tex-newline)
   (tex-cmd structure-type title)
   (tex-newline)
   (tex-newline)
   content
   (tex-newline)))

(defun tex-chapter (title content)
  (generic-tex-strucure 'chapter title content))

(defun tex-section (title content)
  (generic-tex-structure 'section title content))

(defun tex-subsection (title content)
  (generic-tex-structure 'subsection title content))

(defun tex-subsubsection (title content)
  (generic-tex-structure 'subsubsection title content))

;; environments
(defun tex-itemize (item-list &optional one-at-a-time-p)
  (tex-env 'itemize
	   (extract-list-of-strings item-list (concatenate 'string (tex-cmd 'item) " ") (tex-newline))
	   nil
	   (when one-at-a-time-p "<+->")))

(defun tex-figure (filename &optional width caption label)
  (tex-env
   'figure
   (concatenate 'string     (tex-cmd 'includegraphics filename (concatenate 'string "width=" (tex-to-string (if width width 1)) (tex-cmd 'textwidth)))
    (when label (tex-cmd 'label label))
    (when caption (tex-cmd 'caption* caption)))))

;; greek letters TODO

;; math stuff
(defun extract-single-equation-list (eq-list &optional label)
  "extract a single multiline equation, with lines given as a list"
  (concatenate 'string (extract-list-of-strings
	   (butlast eq-list)
	   (concatenate 'string (tex-cmd "nonumber") (tex-newline))
	   (tex-eq-newline))
	  (if (not (null label)) (tex-label label) (tex-cmd "nonumber"))
	  (tex-newline)
	  (car (last eq-list))))

(defun tex-eq (body &optional label punctuation)
  "create a single latex equation, possibly spanning multiple lines"
  (cond ((listp body) (tex-env "align"
			       (concatenate 'string (extract-single-equation-list body label)
				       nil nil nil (if (null punctuation) "," punctuation))))
	(t (tex-env "equation"
		    body
		    nil
		    nil
		    (unless (null label) (tex-label label))
		    (if (null punctuation) "," punctuation)))))

(defun extract-multi-equation-list (eq-list &optional label-list)
  "extract multiline equations, with equations given as a list"
  (concatenate 'string    (extract-list-of-strings
    (mapcar* #'(lambda (eq label)
		 (extract-list-of-strings (list eq) (tex-label label) (tex-eq-newline)))
	     (butlast eq-list)
	     (if (null label-list) (make-list (length (butlast eq-list)) nil) (butlast label-list))))
   (extract-list-of-strings (last eq-list) (tex-label (car (last label-list))) (tex-newline))))

(defun tex-multi-eq (eq-list &optional label-list punctuation)
  "write multiple equations, each one possibly spanning multiple lines"
  (tex-env "align"
	   (concatenate 'string 	    (extract-multi-equation-list eq-list label-list)
	    (if (not (null punctuation)) punctuation ","))))

(defun in-brackets (content)
  (concatenate 'string    (tex-cmd "left(")
   " "
   content
   " "
   (tex-cmd "right)")))

(defun frac (nom denom)
  (tex-cmd 'frac (list nom denom)))

(defun diff (y x &optional diff-symbol order)
  (cond ((equal 'dot diff-symbol) (if (and order (> order 1))
				      (tex-cmd 'dot y)
				    (error "Higher orders not implemented with dots")))
	((equal 'prime diff-symbol) (concatenate 'string 				     (make-exponent
				      y
				      (repeat-string (tex-cmd 'prime) order))))
	(t
	 (if (or (null order) (= order 1))
	     (frac (concatenate 'string (tex-to-string diff-symbol) " " (tex-to-string y))
		   (concatenate 'string (tex-to-string diff-symbol) " " (tex-to-string x)))
	   (frac (concatenate 'string (make-exponent diff-symbol order) " " (tex-to-string y))
		 (make-exponent (concatenate 'string (tex-to-string diff-symbol) " " (tex-to-string x)) order))))))

(defun dd (y x &optional order)
  (diff y x 'd order))

(defun pd (y x &optional order)
  (diff y x (tex-cmd 'partial) order))

;; beamer
(defun beamer-frame (title content)
  (tex-env "frame" content title nil (tex-newline) (tex-newline)))

(defun beamer-set-template (item style)
  (tex-inverse-cmd 'setbeamertemplate item style))

(defun extract-single-equation-list-pause (eq-list &optional omit-newlines-p label)
  "extract a single multiline equation, with lines given as a list"
  (concatenate 'string (extract-list-of-strings
	   (butlast eq-list)
	   (concatenate 'string "\\onslide<+->{" (tex-newline))
	   (concatenate 'string "}" (unless omit-newlines-p (tex-eq-newline))))
	  (concatenate 'string "\\onslide<+->{" (tex-newline))
	  (car (last eq-list))
	  "}"))

(defun tex-eq-pause (body &optional label punctuation)
  "create a single latex equation, possibly spanning multiple lines"
  (cond ((listp body) (tex-env "align*"
			       (concatenate 'string (extract-single-equation-list-pause body label))))
	(t (tex-env "equation"
		    body))))

;; fluid mechanics
;; TODO implement these functions natively
;; (defun um (&rest i)
;;   (cond
;;    ((= 1 (length i)) (make-index (tex-cmd "mean" 'U) i))
;;    (t (make-index 'H i))))
(defun um (&rest i)
  (tex-cmd "um" i))

(defun uf (&rest i)
  (tex-cmd "uf" i))

(defun ua (&rest i)
  (tex-cmd "ua" i))

(defun ud (&rest i)
  (wrap-parens (concat-space (apply #'ua i) "-" (apply #'um i))))

(defun pm ()
  (tex-cmd "pm"))

(defun x (i)
  (make-index 'x i))

(defun xa (i)
  (make-index (tex-cmd 'frak 'x) i))

(defun xd (i)
  (wrap-parens (concat-space (xa i) "-" (x i))))

(defun angular-velocity (index)
  (make-index (tex-cmd 'Omega) index))

(defun kronecker-delta (i j)
  (make-index (tex-cmd 'delta) i j))

(defun permutation-epsilon (i j k)
  (make-index (tex-cmd 'epsilon) i j k))
;; symmetries
(defun trans (var)
  (make-exponent var "*"))

(defun transeq (var)
  (concatenate 'string (trans var) " = "))

(defun tex-symmetry (name expression)
  (concatenate 'string    (if (stringp name)
       (tex-cmd 'infGen (tex-cmd 'text name))
     (tex-cmd 'infGen name))
   " = "
   expression))

(defun tex-simple-symmetry (name diff-var &optional mult-var)
  (tex-symmetry name
		(concatenate 'string mult-var (pd nil diff-var))))

(defun rotmat (index1 index2 &optional axis)
  (make-exponent
   (make-index 'Q (concatenate 'string (tex-to-string index1) (tex-to-string index2)))
   (wrap-brackets (if axis axis (tex-cmd 'alpha)))))

(tex-eval '(tex-eq-pause (list (group (dd (um i) t) + (pd (pm) (x i))))))
(tex-eq-pause (list (concat-space (dd (um 'i) 't) + (pd (pm) (x 'i)))))
(tex-eq "1+1=2")

(tex-to-string 'TaEST)

