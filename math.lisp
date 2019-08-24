;;; commands and environments needed to typeset math in documents


(in-package #:clisptex)

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
		    (if label (tex-label label) (tex-cmd 'nonumber))
		    (if (null punctuation) "," punctuation)))))

(defun extract-multi-equation-list (eq-list &optional label-list)
  "extract multiline equations, with equations given as a list"
  (concatenate 'string
	       (extract-list-of-strings
    (mapcar #'(lambda (eq label)
		 (extract-list-of-strings (list eq) (concat-space (tex-label label) (tex-newline)) (tex-eq-newline)))
	     (butlast eq-list)
	     (if (null label-list) (make-list (length (butlast eq-list))) (butlast label-list))))
   (extract-list-of-strings (last eq-list) (concat-space (tex-label (car (last label-list))) (tex-newline)) (tex-newline))))

(defun tex-multi-eq (eq-list &optional label-list punctuation)
  "write multiple equations, each one possibly spanning multiple lines"
  (tex-env "align"
	   (concatenate 'string
			(extract-multi-equation-list eq-list label-list)
	    (if (not (null punctuation)) punctuation ","))))

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

(defun kronecker-delta (i j)
  (make-index (tex-cmd 'delta) i j))

(defun permutation-epsilon (i j k)
  (make-index (tex-cmd 'epsilon) i j k))

(defun equal? ()
  (tex-cmd 'stackrel (list "?" "=")))

(defun equal! ()
  (tex-cmd 'stackrel (list "!" "=")))

(defun lcos (&rest args)
  (tex-cmd 'cos args))

(defun lsin (&rest args)
  (tex-cmd 'sin args))

(defun ltan (&rest args)
  (tex-cmd 'tan args))

(defun normal-vector (index)
  (make-index "n" index))

(defun power (base exponent)
  (make-exponent base exponent))

(defun square (&rest base)
  (power (extract-list-of-strings base) 2))
