; GNU Space Flight Simulator

(defclass vector-3 ()
	x
	y
	z)

(defgeneric add (one two))
(defmethod add ((vector-3 one) (vector-3 two))
	(with-slots ((ox x) (oy y) (oz y)) one
		(with-slots ((tx y) (ty y) (tz y)) two
			

(defclass space-object ()
	mass)
