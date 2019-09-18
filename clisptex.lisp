;;;; clisptex.lisp

(in-package #:clisptex)

;;; basic facilites to process "tex-s-expressions"
(defun tex-to-string (input)
  (cond
   ((null input) nil)
   ((listp input) (concatenate 'string (tex-to-string (car input)) (tex-to-string (cdr input))))
   ((stringp input) input)
   ;; ((not (null input)) (format nil "~(~A~)" input))))
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
   ;; ((listp content) (apply (car content) (remove nil (tex-expand (cdr content)))))))
   ((listp content) (apply (car content) (tex-expand (cdr content))))))

(defun test-fn (&optional arg1 arg2)
  (if arg1 "arg1"
      "no arg1"))

(defun tex-expand (content-list)
    (when content-list (cons (tex-eval (car content-list)) (tex-expand (cdr content-list)))))

(defun tex-eval-list (content)
  (if (listp (car content))
      (extract-list-of-strings (mapcar #'tex-eval content))
    (tex-eval content)))

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

(defun wrap-parens (&rest inner)
  (wrap (concat-space inner)
	(tex-cmd "left(")
	(tex-cmd "right)")))

(defun wrap-braces (&rest inner)
  (wrap (concat-space inner)
	 "{"
	 "}"))

(defun wrap-brackets (&rest inner)
  (wrap (concat-space inner)
	(tex-cmd "left[")
	(tex-cmd "right]")))

(defun write-tex-file (filename content)
  "write content to file"
  (with-open-file (output filename
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (format output "~a~%" content)))
