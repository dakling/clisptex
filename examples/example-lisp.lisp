(ql:quickload :clisptex)

(in-package :clisptex)

;;; this example shows how to generate an entire document from lisp
(write-tex-file
 "example-lisp.tex"
 (tex-eval-list
  '((tex-documentclass article)
    (tex-document
     (group
      (tex-section
       (group
	Create a latex document from lisp)
       (group
	In this document we show how to create an entire latex document from lisp.
	It is recommended to use org-mode instead.))
      (tex-section "Using strings"
       (group
	"Strings can be used if you prefer, and if you want to use commas and have 
case-sensitivity. Perhaps it is more convenient not to use tex-eval-list on the entire document, but only on the important parts.
Since I don't really use this workflow personally, I am not sure :)"
	(tex-eq 1+1=2))))))))
