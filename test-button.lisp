(defclass test-button (button)
	((red
		:initarg :red
		:initform 1)
	(green
		:initarg :green
		:initform 1)
	(blue
		:initarg :blue
		:initform 1)
	(alpha
		:initarg :alpha
		:initform 1)))

(defmethod draw-2d ((button test-button) screen-width screen-height)
	(with-slots (red green blue alpha) button
		(gl-matrix-mode *gl-projection*)
		(gl-load-identity)
		(glu-ortho2-d 0 1 0 1)
		(gl-matrix-mode *gl-modelview*)
		(gl-load-identity)
		(gl-clear *gl-depth-buffer-bit*)
		(gl-color4f red green blue alpha)
		(gl-begin-end *gl-quads*
			(gl-vertex2d 0 0)
			(gl-vertex2d 0 1)
			(gl-vertex2d 1 1)
			(gl-vertex2d 1 0))))
