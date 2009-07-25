(defclass button (box-2d)
	((size
		:initarg :size
		:initform (make-vector-2 40 40)
		:documentation "The size of the button in pixels as a vector-2.")
	(click-function
		:initarg :click-function
		:initform nil
		:documentation "The function called when the button is clicked")))

(defmethod compute-sw-corner ((button button) screen-size)
	(with-slots (anchor-point pos size) button
		(sub (mult pos screen-size) (mult anchor-point size))))

(defmethod draw-2d :around ((button button) screen-size) ; Make an appropriate viewport
	(with-slots (size) button
		(let ((sw-corner (compute-sw-corner button screen-size)))
			(with-slots (x y) sw-corner
				(with-slots ((width x) (height y)) size
					(gl-viewport x y width height)))))
	(call-next-method))

(defmethod check-and-handle-click ((button button) mouse-button state click-pos screen-size)
	(with-slots (size click-function) button
		(let ((sw-corner (compute-sw-corner button screen-size)))
			(with-slots (x y) sw-corner
				(with-slots ((click-x x) (click-y y)) click-pos
					(with-slots ((width x) (height y)) size
						(if (and
								(< x click-x (+ x  width))
								(< y click-y (+ y height))
								(= 0 mouse-button)
								(= *glut-down* state)
								click-function)
							(progn (funcall click-function)))))))))
