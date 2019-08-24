;;; environments and math stuff needed in beamer presentations

(in-package #:clisptex)


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
	(t (tex-env "equation*"
		    body))))

(defun on-slide (content &optional count-from count-to)
  (tex-cmd 'onslide
	   content
	   nil
	   (cond ((and count-from count-to) (concatenate 'string 
							(write-to-string count-from)
							"-"
							(write-to-string count-to)))
		 (count-from (concatenate 'string
					  (write-to-string count-from)
					  "-"))
		 (count-to (concatenate 'string
					"-"
					(write-to-string count-to)))
		 (t "+-"))))

;; TODO (tex-eval '(on-slide test))

(defstruct beamer-node
  name
  content
  connection
  pos-x
  pos-y)

(defstruct beamer-node-connection
  content
  from-node
  to-node)

;; (make-beamer-node :name "test-node" :content "some text")
;; (make-beamer-node :name "test-node" :content "some text" :to test-node-2)
;; (make-beamer-node :name "test-node-2" :content "some more text" :from test-node-2)

;; three steps:
;; (1) define nodes and connection
;; (2) arange them nicely
;; (3) worry about onslide stuff
