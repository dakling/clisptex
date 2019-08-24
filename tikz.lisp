;;; needed for tikz

(in-package #:clisptex)

(defstruct content
  text
  overlay)

(defstruct node
  content-list
  overlay-list
  (style "block")
  pos-x
  pos-y)

(defstruct path
  from-node
  to-node
  (style "line")
  annotation
  annotation-position)

(defun tikz-setup ()
  nil)

(defun tikz-picture ()
  (tikz-setup)
  )

(defun tikzstyle (name content)
  (concat-space (tex-cmd 'tikzstyle name)
		"="
		(wrap content "[" "]")))

;; try this stuff out

(setq exact (make-node :style "bigblock" :pos-x 0 :pos-y 0)))

(setq var-list (make-node :pos-x 7 :pos-y -3))

(setq symmetries (make-node :pos-x 7 :pos-y 0))

(setq lie-theory (make-node))

(make-path :from-node symmetries :to-node exact :annotation lie-theory)

(setq exact-content-list
      (list
       (make-content :text "-" :overlay 4)))
