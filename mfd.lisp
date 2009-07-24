(defclass mfd (box-2d)
	((max-width
		:initarg :max-width
		:initform .5
		:documentation "The maximum width of the MFD, expressed as a fraction of the screen width. The MFD will be square.")
	(max-height
		:initarg :max-height
		:initform .5
		:documentation "The maximum height of the MFD, expressed as a fraction of the screen height. The MFD will be square.")))

(defgeneric compute-size (mfd screen-width screen-height))

(defmethod compute-size ((mfd mfd) screen-width screen-height)
	(with-slots (max-width max-height) mfd
		(loop for size in (list (* max-width screen-width) (* max-height screen-height)) minimizing size)))

(defmethod compute-sw-corner ((mfd mfd) screen-width screen-height)
	(with-slots (ap-x ap-y x y) mfd
		(let ((size (compute-size mfd screen-width screen-height)))
			(list
				(* (- x (* ap-x (/ size screen-width)))  screen-width)
				(* (- y (* ap-y (/ size screen-height))) screen-height)))))

(defmethod draw-2d :around ((mfd mfd) screen-width screen-height) ; I give unto thee a viewport. Do what you wish with it.
	(let ((size (compute-size mfd screen-width screen-height)) (sw-corner (compute-sw-corner mfd screen-width screen-height)))
		(gl-viewport
			(first  sw-corner)
			(second sw-corner)
			size
			size))
	(call-next-method))
