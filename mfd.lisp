(defclass mfd (box-2d)
	((max-size
		:initarg :max-size
		:initform (make-vector-2 .4 .4)
		:documentation "The maximum size of the MFD, expressed as a fraction of the screen width. The MFD will be square, with sides as long as the shorter of the two specified values.")))

(defgeneric compute-size (mfd screen-size))

(defmethod compute-size ((mfd mfd) screen-size)
	(with-slots ((screen-width x) (screen-height y)) screen-size
		(with-slots ((max-width x) (max-height y)) (slot-value mfd 'max-size)
			(loop for size in (list (* max-width screen-width) (* max-height screen-height)) minimizing size))))

(defmethod compute-sw-corner ((mfd mfd) screen-size)
	(with-slots (anchor-point pos) mfd
		(let ((size (compute-size mfd screen-size)))
			(sub (mult pos screen-size) (mult size anchor-point)))))

(defmethod draw-2d :around ((mfd mfd) screen-size) ; I give unto thee a viewport. Do what you wish with it.
	(let ((size (compute-size mfd screen-size)) (sw-corner (compute-sw-corner mfd screen-size)))
		(with-slots (x y) sw-corner
			(gl-viewport x y size size)))
	(call-next-method))
