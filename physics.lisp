; GNU Space Flight Simulator

(defclass vector-3 ()
	(x
	y
	z))

(defgeneric print-vec (vec)
	(:documentation "print."))

(defmethod print-vec ((vec vector-3))
	(with-slots (x y z) vec
		(format t "(~a ~a ~a)~%" x y z)))

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
		"acceleration measured in m/s/s, cartesian, as a vector-3")))

(defgeneric compute-gravity (obj all-objs)
	(:documentation "compute gravity on obj due to all-objs."))

(defmethod compute-gravity ((obj space-object) all-objs)
	(loop for other-obj in all-objs do
		(unless (eq obj other-obj)
			(let*
					((rel-pos (sub
						(slot-value other-obj 'pos)
						(slot-value obj 'pos)))
					(distance (magnitude rel-pos)))
				(unless (= 0.0 distance)
					(setf (slot-value obj 'acc) (add (slot-value obj 'acc)
						(mult
							(/ ; G * m / r^2
								(* *G* (slot-value other-obj 'mass))
								(expt distance 2))
							(mult ; r hat
								(/ 1 distance)
								rel-pos)))))))))

(defgeneric integrate-acc-to-vel (obj dt)
	(:documentation "integrate acceleration to get velocity."))

(defmethod integrate-acc-to-vel ((obj space-object) dt)
	(with-slots (vel acc) obj
		(setf vel (add vel (mult dt acc)))))

(defgeneric integrate-vel-to-pos (obj dt)
	(:documentation "integrate velocity to get position."))

(defmethod integrate-vel-to-pos ((obj space-object) dt)
	(with-slots (pos vel) obj
		(setf pos (add pos (mult dt vel)))))
