(defclass mfd ()
	((ap-x
		:initarg :ap-x
		:initform 0
		:documentation "X coordinate of the anchor point of the MFD, in MFD coordinates, expressed as a fraction of the mfd size")
	(ap-y
		:initarg :ap-y
		:initform 0
		:documentation "Y coordinate of the anchor point of the MFD, in MFD coordinates, expressed as a fraction of the mfd size")
	(x
		:initarg :x
		:initform .25
		:documentation "The x coordinate of the anchor point of the MFD, in window coordinates, expressed as a fraction of the screen width")
	(y
		:initarg :y
		:initform .25
		:documentation "The y coordinate of the anchor point of the MFD, in window coordinates, expressed as a fraction of the screen height")
	(max-width
		:initarg :max-width
		:initform .5
		:documentation "The maximum width of the MFD, expressed as a fraction of the screen width. The MFD will be square.")
	(max-height
		:initarg :max-height
		:initform .5
		:documentation "The maximum height of the MFD, expressed as a fraction of the screen height. The MFD will be square.")))

(defgeneric draw-mfd (mfd screen-width screen-height)
	(:documentation "Draw an mfd."))

(defmethod draw-mfd :around ((mfd mfd) screen-width screen-height) ; I give unto thee a viewport. Do what you wish with it.
	(with-slots (ap-x ap-y x y max-width max-height) mfd
		(let ((size (loop for size in (list (* max-width screen-width) (* max-height screen-height)) minimizing size)))
			(gl-viewport
				(* (- x (* ap-x (/ size screen-width)))  screen-width)
				(* (- y (* ap-y (/ size screen-height))) screen-height)
				size
				size)))
	(call-next-method))
