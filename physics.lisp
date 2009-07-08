; YINO = YINO it's not Orbiter
(defclass vector-3 ()
	(x y z))

(defun make-vector-3 (&optional (x 0.0) (y 0.0) (z 0.0))
	(let ((vec (make-instance 'vector-3)))
		(with-slots ((vx x) (vy y) (vz z)) vec
			(setf vx x)
			(setf vy y)
			(setf vz z))
		vec))

(defgeneric print-vec (vec out-stream)
	(:documentation "print."))

(defmethod print-vec ((vec vector-3) out-stream)
	(with-slots (x y z) vec
		(format out-stream "(~a ~a ~a)~%" x y z)))

(defgeneric add (one two)
	(:documentation "addition"))

(defmethod add ((one vector-3) (two vector-3))
	(with-slots ((ox x) (oy y) (oz z)) one
		(with-slots ((tx x) (ty y) (tz z)) two
			(make-vector-3
				(+ ox tx)
				(+ oy ty)
				(+ oz tz)))))

(defgeneric mult (scalar something)
	(:documentation "scalar multiplication"))

(defmethod mult (scalar (vec vector-3))
	(with-slots (x y z) vec
		(make-vector-3
			(* scalar x)
			(* scalar y)
			(* scalar z))))

(defgeneric dot (one two)
	(:documentation "dot product"))

(defmethod dot ((one vector-3) (two vector-3))
	(with-slots ((ox x) (oy y) (oz z)) one
		(with-slots ((tx x) (ty y) (tz z)) two
			(+
				(* ox tx)
				(* oy ty)
				(* oz tz)))))

(defgeneric cross (one two)
	(:documentation "cross product"))

(defmethod cross ((one vector-3) (two vector-3))
	(with-slots ((ox x) (oy y) (oz z)) one
		(with-slots ((tx x) (ty y) (tz z)) two
			(make-vector-3
				(- (* oy tz) (* oz ty))
				(- (* oz tx) (* ox tz))
				(- (* ox ty) (* oy tx))))))

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

(defclass quaternion ()
	(w x y z))

(defun make-quaternion (&optional (w 1.0) (x 0.0) (y 0.0) (z 0.0))
	(let ((quat (make-instance 'quaternion)))
		(with-slots ((qw w) (qx x) (qy y) (qz z)) quat
			(setf qw w)
			(setf qx x)
			(setf qy y)
			(setf qz z))
		quat))

(defmethod add ((one quaternion) (two quaternion))
	(with-slots ((ow w) (ox x) (oy y) (oz z)) one
		(with-slots ((tw w) (tx x) (ty y) (tz z)) two
			(make-quaternion
				(+ ow tw)
				(+ ox tx)
				(+ oy ty)
				(+ oz tz)))))

(defmethod mult ((one quaternion) (two quaternion))
	(with-slots ((ow w) (ox x) (oy y) (oz z)) one
		(with-slots ((tw w) (tx x) (ty y) (tz z)) two
			(make-quaternion
				(+ (* ow tw) (* -1 ox tx) (* -1 oy ty) (* -1 oz tz))
				(+ (* ow tx) (*    ox tw) (*    oy tz) (* -1 oz ty))
				(+ (* ow ty) (* -1 ox tz) (*    oy tw) (*    oz tx))
				(+ (* ow tz) (*    ox ty) (* -1 oy tx) (*    oz tw))))))

(defmethod mult (scalar (quat quaternion))
	(with-slots (w x y z) quat
		(make-quaternion
			(* scalar w)
			(* scalar x)
			(* scalar y)
			(* scalar z))))

(defmethod magnitude ((quat quaternion))
	(with-slots (w x y z) quat
		(expt
			(+
				(expt w 2)
				(expt x 2)
				(expt y 2)
				(expt z 2))
			0.5)))

(defgeneric normalize (unnormalized)
	(:documentation "Normalize something, like a vector, or a quaternion"))

(defmethod normalize ((quat quaternion))
	(let ((len (magnitude quat)))
		(with-slots (w x y z) quat
			(if (/= len 0)
				(progn
					(setf w (/ w len))
					(setf x (/ x len))
					(setf y (/ y len))
					(setf z (/ z len)))
				(progn
					(setf w 1)
					(setf x 0)
					(setf y 0)
					(setf z 0))))))

(defclass matrix-3-3 ()
	(m11 m12 m13
	m21 m22 m23
	m31 m32 m33))

(defun make-matrix-3-3 (&optional
		(m11 0) (m12 0) (m13 0)
		(m21 0) (m22 0) (m23 0)
		(m31 0) (m32 0) (m33 0))
	(let ((mat (make-instance 'matrix-3-3)))
		(with-slots
				((mm11 m11) (mm12 m12) (mm13 m13)
				(mm21 m21) (mm22 m22) (mm23 m23)
				(mm31 m31) (mm32 m32) (mm33 m33)) mat
			(setf mm11 m11)
			(setf mm12 m12)
			(setf mm13 m13)
			(setf mm21 m21)
			(setf mm22 m22)
			(setf mm23 m23)
			(setf mm31 m31)
			(setf mm32 m32)
			(setf mm33 m33))
		mat))

(defmethod mult ((mat matrix-3-3) (vec vector-3))
	(with-slots (x y z) vec
		(with-slots
				(m11 m12 m13
				m21 m22 m23
				m31 m32 m33) mat
			(make-vector-3
				(+
					(* x m11)
					(* y m12)
					(* z m13))
				(+
					(* x m21)
					(* y m22)
					(* z m23))
				(+
					(* x m31)
					(* y m32)
					(* z m33))))))

(defvar *G* 6.673e-11 (:documentation "Gravitational constant"))
(defvar *pi* 3.14159265358979323 (:documentation "Tasty pie"))

(defclass space-object ()
	((mass :documentation "Mass measured in kg.")

	(pos :documentation
		"position measured in m, cartesian, as a vector-3")
	(vel :documentation
		"velocity measured in m/s, cartesian, as a vector-3")
	(acc :documentation
		"acceleration measured in m/s/s, cartesian, as a vector-3")

	(inertia-tensor :documentation
		"inertia tensor, as a matrix-3-3")
	(ang-pos :documentation
		"angular position, as a quaternion")
	(ang-vel :documentation
		"angular velocity in radians/s, as a vector-3")
	(ang-acc :documentation
		"angular acceleration in radians/s/s, as a vector-3")))

(defun make-space-object (&key
		(mass 1.0)
		(pos (make-vector-3))
		(vel (make-vector-3))
		(acc (make-vector-3))
		(inertia-tensor (compute-inertia-tensor 1 1 1))
		(ang-pos (make-quaternion))
		(ang-vel (make-vector-3))
		(ang-acc (make-vector-3)))
	(let ((obj (make-instance 'space-object)))
		(with-slots
				((omass mass)
				(opos pos)
				(ovel vel)
				(oacc acc)
				(oinertia-tensor inertia-tensor)
				(oang-pos ang-pos)
				(oang-vel ang-vel)
				(oang-acc ang-acc)) obj
			(setf omass mass)
			(setf opos pos)
			(setf ovel vel)
			(setf oacc acc)
			(setf oinertia-tensor inertia-tensor)
			(setf oang-pos ang-pos)
			(setf oang-vel ang-vel)
			(setf oang-acc ang-acc))
		obj))

(defun compute-inertia-tensor (x y z)
	(make-matrix-3-3
		x 0 0
		0 y 0
		0 0 z))

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
						(mult ; gravitational field vector
							(/ ; G * m / r^2
								(* *G* (slot-value other-obj 'mass))
								(expt distance 2))
							(mult ; r hat
								(/ 1 distance)
								rel-pos)))))))))

(defgeneric compute-acc (obj all-objs)
	(:documentation "compute the acceleration on obj, and update acc"))

(defmethod compute-acc ((obj space-object) all-objs)
	(setf (slot-value obj 'acc) (make-vector-3 0 0 0))
	(compute-gravity obj all-objs))

(defgeneric integrate-acc-to-vel (obj dt)
	(:documentation "integrate acceleration to get velocity."))

(defmethod integrate-acc-to-vel ((obj space-object) dt)
	(with-slots (vel acc) obj
		(setf vel (add vel (mult dt acc)))))

(defgeneric integrate-ang-acc-to-ang-vel (obj dt)
	(:documentation "integrate angular acceleration to get angular velocity."))

(defmethod integrate-ang-acc-to-ang-vel ((obj space-object) dt)
	(with-slots (ang-vel ang-acc) obj
		(setf ang-vel (add ang-vel (mult dt ang-acc)))))

(defgeneric integrate-vel-to-pos (obj dt)
	(:documentation "integrate velocity to get position."))

(defmethod integrate-vel-to-pos ((obj space-object) dt)
	(with-slots (pos vel) obj
		(setf pos (add pos (mult dt vel)))))

(defgeneric integrate-ang-vel-to-ang-pos (obj dt)
	(:documentation "integrate angular velocity to get angular position."))

(defmethod integrate-ang-vel-to-ang-pos ((obj space-object) dt)
	(with-slots (ang-pos ang-vel) obj
		(with-slots (x y z) ang-vel
			;(print (list  x y z))
			;(with-slots (w x y z) (mult 0.5 (mult (make-quaternion 0 x y z) ang-pos))
			;	(print (list w x y z)))
			(setf ang-pos (add ang-pos (mult dt (mult 0.5 (mult (make-quaternion 0 x y z) ang-pos)))))
			(normalize ang-pos))))

(defun timestep (all-objs dt)
	(loop for obj in all-objs do
		(compute-acc obj all-objs))
	(loop for obj in all-objs do
		(integrate-acc-to-vel obj dt)
		(integrate-ang-acc-to-ang-vel obj dt)
		(integrate-vel-to-pos obj dt)
		(integrate-ang-vel-to-ang-pos obj dt)))

(defun print-timestep (all-objs)
	(format t "begin-timestep~%")
	(dolist (obj all-objs)
		(format t "begin-object~%")
		(with-slots (x y z) (slot-value obj 'pos)
			(format t "pos ~a ~a ~a~%" x y z))
		(with-slots (w x y z) (slot-value obj 'ang-pos)
				(let ((len (magnitude (make-vector-3 x y z))))
					(print (list w x y z len))
					(if (/= 0 len) ; print in angle-axis form (in degrees, because that's what OpenGL uses *shudder*)
						(format t "ang-pos ~a ~a ~a ~a~%"
							(/ (* (* 2 (acos w)) 180) *pi*)
							(/ x len)
							(/ y len)
							(/ z len))
						(format t "ang-pos ~a ~a ~a ~a~%"
							0
							1
							0
							0))))
		(format t "radius 1~%")
		(format t "end-object~%"))
	(format t "end-timestep~%"))

(defun main-loop (time-acceleration all-objs)
	(without-floating-point-underflow
		(let ((current-time (get-internal-real-time)) prev-time)
			(loop
				(setf prev-time current-time)
				(setf current-time (get-internal-real-time))
				(timestep all-objs (/ (- current-time prev-time) (/ internal-time-units-per-second time-acceleration)))
				(format *error-output* "dt: ~a~%" (+ 0.0 (/ (- current-time prev-time) (/ internal-time-units-per-second time-acceleration))))
				(print-timestep all-objs)))))
