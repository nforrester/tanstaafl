(defclass test-mfd (mfd)
	((color
		:initarg :color
		:initform (make-color 1 1 1 1))))

(defmethod draw-2d (mfd screen-size)
	(gl-matrix-mode *gl-projection*)
	(gl-load-identity)
	(glu-ortho2-d 0 1 0 1)
	(gl-matrix-mode *gl-modelview*)
	(gl-load-identity)
	(gl-clear *gl-depth-buffer-bit*)
	(gl-color (slot-value mfd 'color))
	(gl-begin-end *gl-quads*
		(gl-vertex2d 0 0)
		(gl-vertex2d 0 1)
		(gl-vertex2d 1 1)
		(gl-vertex2d 1 0))
	(gl-color (make-color 1 1 1 1))
	(gl-place-string (concatenate 'string `(,@(concatenate 'list "JELLO WORLD") #\Newline ,@(concatenate 'list "helo"))) (make-vector-2 .5 .5))
	(gl-place-string "jello world" (make-vector-2 .5 .6)))
