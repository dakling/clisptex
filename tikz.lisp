;;; needed for tikz

(in-package #:clisptex)

(defstruct point
  (pos-x 0)
  (pos-y 0))

(defstruct node
  content
  label
  (style "draw")
  (width "20em")
  option-list 
  location
  on-slides)

(defstruct path
  from-node
  to-node
  (style "line")
  option-list 
  annotation
  annotation-position)

(defun only (slide content)
  (tex-cmd 'only content nil slide))

(defun onslide (slide content)
  (tex-cmd 'onslide content nil slide))

(defun tikz-picture (content &optional show-grid-p)
  (tex-env "tikzpicture" content nil (when show-grid-p "show background grid")))

(defun draw-node (node)
  (concatenate 'string
               (tex-cmd 'node nil (cons
                                   (concatenate 'string "text width=" (node-width node))
                                   (let ((options (node-option-list node)))
                                     (when options options))))
               " ("
               (node-label node)
               ") at ("
               (write-to-string (point-pos-x (node-location node)))
               ","
               (write-to-string (point-pos-y (node-location node)))
               ") "
               (wrap-braces
                (when (node-on-slides node)
                  (onslide (write-to-string (node-on-slides node)) nil))
                (wrap-braces (node-content node)))
               ";"))

(draw-node
 (make-node
  :content "This is a test node"
  :label "test-node"
  :location (make-point :pos-x 1 :pos-y 2)
  :on-slides 2))

(defun block (content &optional width)
  (make-node ))

;; try this stuff out

(setq exact (make-node :style "bigblock" :pos-x 0 :pos-y 0)))

(setq var-list (make-node :pos-x 7 :pos-y -3))

(setq symmetries (make-node :pos-x 7 :pos-y 0))

(setq lie-theory (make-node))

(make-path :from-node symmetries :to-node exact :annotation lie-theory)

(setq exact-content-list
      (list
       (make-content :text "-" :overlay 4)))
