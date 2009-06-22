; GNU Space Flight Simulator

(defclass vector-3 ()
	(x
	y
	z))

(defgeneric add (one two))
;	:documentation "addition")

(defmethod add ((one vector-3) (two vector-3))
	(with-slots ((ox x) (oy y) (oz z)) one
		(with-slots ((tx x) (ty y) (tz z)) two
			(let ((result (make-instance 'vector-3)))
				(with-slots ((rx x) (ry y) (rz z)) result
					(setf rx (+ ox tx))
					(setf ry (+ oy ty))
					(setf rz (+ oz tz))
					result)))))

(defgeneric mult (scalar something))
;	:documentation "scalar multiplication")

(defmethod mult (scalar (vec vector-3))
	(with-slots (x y z) vec
		(let ((result (make-instance 'vector-3)))
			(with-slots ((rx x) (ry y) (rz z)) result
				(setf rx (* scalar x))
				(setf ry (* scalar y))
				(setf rz (* scalar z))))))

(defgeneric sub (one two))
;	:documentation "subtraction")

(defmethod sub ((one vector-3) (two vector-3))
	(add one (mult -1 two)))

(defvar *G* 5.9e-11) ; Gravitational constant

(defclass space-object ()
	((mass :documentation "Mass measured in kg.")
	(pos :documentation
		"position measured in m, cartesian, as a vector-3")
	(vel :documentation
		"velocity measured in m/s, cartesian, as a vector-3")
	(acc :documentation
		"accelleration measured in m/s/s, cartesian, as a vector-3")))
