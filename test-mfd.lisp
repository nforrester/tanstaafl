(defclass test-mfd (mfd)
	((red
		:initarg :red
		:initform 1)
	(green
		:initarg :green
		:initform 1)
	(blue
		:initarg :blue
		:initform 1)))

(defmethod draw-mfd (mfd screen-width screen-height)
	(with-slots (red green blue) mfd
		(gl-matrix-mode *gl-projection*)
		(gl-load-identity)
		(glu-ortho2-d 0 1 0 1)
		(gl-matrix-mode *gl-modelview*)
		(gl-load-identity)
		(gl-clear *gl-depth-buffer-bit*)
		(gl-color3f red green blue)
		(gl-begin-end *gl-lines*
			(gl-vertex2d 0 0)
			(gl-vertex2d 1 1)
			(gl-vertex2d 0 1)
			(gl-vertex2d 1 0))))
