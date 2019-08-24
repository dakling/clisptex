;;; provide low-level abstractions for latex commands

;;; most of the time, it is probably poor style to use this in your document directly -
;;; instead, you should create your own higher-level semantic functions using the
;;; functions defined here.

(in-package #:clisptex)

(defun make-exponent (main-expression exponent)
  (concatenate 'string (tex-to-string main-expression) "^" (wrap-braces exponent)))

(defun make-index (main-expression &rest indices)
  (concatenate 'string (tex-to-string main-expression) "_" (wrap-braces (extract-list-of-strings indices))))

(defun tex-newline () (string #\Newline))

(defun tex-eq-newline () (concatenate 'string "\\\\" (tex-newline)))

(defun tex-cmd (cmd-name &optional mandatory-args optional-args triangle-args)
  "create a tex command accepting single arguments or multiple arguments as a list"
  (let ((mandatory-arg-list
	  (list-if-not-already mandatory-args))
	(optional-arg-list
	  (list-if-not-already optional-args))
	(triangle-arg-list
	  (list-if-not-already triangle-args)))
    (concatenate 'string "\\"
	    (tex-to-string cmd-name)
	    (extract-list-of-strings (remove nil triangle-arg-list) "<" ">")
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

(defun tex-math (&rest content)
  (wrap (concat-space content) (tex-cmd "(") (tex-cmd ")")))
