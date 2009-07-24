(defclass button (box-2d)
	((width
		:initarg :width
		:initform 40
		:documentation "The width of the button in pixels.")
	(height
		:initarg :height
		:initform 40
		:documentation "The height of the button in pixels.")
	(click-function
		:initarg :click-function
		:initform nil
		:documentation "The function called when the button is clicked")))

(defmethod compute-sw-corner ((button button) screen-width screen-height)
	(with-slots (ap-x ap-y x y width height) button
		(list
			(- (* x screen-width)  (* ap-x width))
			(- (* y screen-height) (* ap-y height)))))

(defmethod draw-2d :around ((button button) screen-width screen-height) ; Make an appropriate viewport
	(with-slots (width height) button
		(let ((sw-corner (compute-sw-corner button screen-width screen-height)))
			(gl-viewport
				(first  sw-corner)
				(second sw-corner)
				width
				height)))
	(call-next-method))

(defmethod check-and-handle-click ((button button) mouse-button state click-x click-y screen-width screen-height)
	(with-slots (width height click-function) button
		(let*
				((sw-corner (compute-sw-corner button screen-width screen-height))
				(x (first sw-corner))
				(y (second sw-corner)))
			(if (and
					(< x click-x (+ x  width))
					(< y click-y (+ y height))
					(= 0 mouse-button)
					(= *glut-down* state)
					click-function)
				(progn (funcall click-function))))))
