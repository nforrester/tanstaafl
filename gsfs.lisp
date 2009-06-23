; GNU Space Flight Simulator

(defclass vector-3 ()
	(x
	y
	z))

(defgeneric add (one two)
	(:documentation "addition"))

(defmethod add ((one vector-3) (two vector-3))
	(with-slots ((ox x) (oy y) (oz z)) one
		(with-slots ((tx x) (ty y) (tz z)) two
			(let ((result (make-instance 'vector-3)))
				(with-slots ((rx x) (ry y) (rz z)) result
					(setf rx (+ ox tx))
					(setf ry (+ oy ty))
					(setf rz (+ oz tz))
					result)))))

(defgeneric mult (scalar something)
	(:documentation "scalar multiplication"))

(defmethod mult (scalar (vec vector-3))
	(with-slots (x y z) vec
		(let ((result (make-instance 'vector-3)))
			(with-slots ((rx x) (ry y) (rz z)) result
				(setf rx (* scalar x))
				(setf ry (* scalar y))
				(setf rz (* scalar z))
				result))))

(defgeneric sub (one two)
	(:documentation "subtraction"))

(defmethod sub ((one vector-3) (two vector-3))
	(add one (mult -1 two)))

(defgeneric magnitude (vec)
	(:documentation "magnitude of a vector"))

(defmethod magnitude ((vec vector-3))
	(with-slots (x y z) vec
		(expt (+
			(expt x 2)
			(expt y 2)
			(expt z 2)) 0.5)))

(defvar *G* 6.673e-11 (:documentation "Gravitational constant"))

(defclass space-object ()
	((mass :documentation "Mass measured in kg.")
	(pos :documentation
		"position measured in m, cartesian, as a vector-3")
	(vel :documentation
		"velocity measured in m/s, cartesian, as a vector-3")
	(acc :documentation
		"accelleration measured in m/s/s, cartesian, as a vector-3")))

(defgeneric compute-gravity (obj all-objs)
	(:documentation "compute gravity on obj due to all-objs."))

;(defmethod compute-gravity ((obj space-object) all-objs)
;	(loop for other-obj in all-objs do
;		(if (not (eq obj other-obj))
;			(setf acc (add acc
;				(/
;					(* *G* (slot-value other-obj 'mass))
;					(magnitude 
