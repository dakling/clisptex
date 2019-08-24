;;; latex commands and environments frequently used in all contexts

(in-package #:clisptex)


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

;; references, citations
(defun tex-label (label)
  (unless (null label) (concatenate 'string (tex-cmd "label" label))))
