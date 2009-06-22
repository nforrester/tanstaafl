; GNU Space Flight Simulator

(defclass vector-3 ()
	(x
	y
	z))

(defgeneric add (one two))

(defmethod add ((one vector-3) (two vector-3))
	(with-slots ((ox x) (oy y) (oz z)) one
		(with-slots ((tx x) (ty y) (tz z)) two
			(let ((result (make-instance 'vector-3)))
				(with-slots ((rx x) (ry y) (rz z)) result
					(setf rx (+ ox tx))
					(setf ry (+ oy ty))
					(setf rz (+ oz tz))
					result)))))

(defclass space-object ()
	(mass))
