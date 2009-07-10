(defvar *G* 6.673e-11 (:documentation "Gravitational constant"))

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
					(add-force obj
						(mult ; gravitational force vector
							(/ ; G * m / r^2
								(* *G* (slot-value obj 'mass) (slot-value other-obj 'mass))
								(expt distance 2))
							(mult ; r hat
								(/ 1 distance)
								rel-pos)) :frame :global))))))

(defgeneric add-force (obj force &key)
	(:documentation "apply a force to obj, and update acc"))

(defmethod add-force ((obj space-object) (force vector-3) &key (frame :local))
	(with-slots (mass acc) obj
		(setf acc (add acc (mult (/ 1 mass)
			(if (eq frame :global)
				force
				(rotate force (slot-value obj 'ang-pos))))))))

(defgeneric add-torque (obj torque &key)
	(:documentation "apply a torque to obj, and update acc"))

(defmethod add-torque ((obj space-object) (torque vector-3) &key (frame :local))
	(format *error-output* "add-torque!~%")
	(with-slots (inertia-tensor ang-acc) obj
		(setf ang-acc (add ang-acc (mult (inverse inertia-tensor)
			(if (eq frame :global)
				torque
				(rotate torque (slot-value obj 'ang-pos))))))))

(defgeneric compute-acc (obj all-objs)
	(:documentation "compute the acceleration on obj, and update acc"))

(defmethod compute-acc ((obj space-object) all-objs)
	(setf (slot-value obj 'acc) (make-vector-3 0 0 0))
	(compute-gravity obj all-objs))

(defgeneric compute-ang-acc (obj all-objs)
	(:documentation "compute the angular acceleration on obj, and update ang-acc"))

(defmethod compute-ang-acc ((obj space-object) all-objs)
	(setf (slot-value obj 'ang-acc) (make-vector-3 0 0 0)))

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
			(setf ang-pos (add ang-pos (mult dt (mult 0.5 (mult (make-quaternion 0 x y z) ang-pos)))))
			(normalize ang-pos))))

(defun timestep (all-objs dt)
	(loop for obj in all-objs do
		(compute-acc obj all-objs)
		(compute-ang-acc obj all-objs))
	(loop for obj in all-objs do
		(integrate-acc-to-vel obj dt)
		(integrate-ang-acc-to-ang-vel obj dt)
		(integrate-vel-to-pos obj dt)
		(integrate-ang-vel-to-ang-pos obj dt)))

(defun print-timestep (all-objs)
	(format *state-output-stream* "begin-timestep~%")
	(dolist (obj all-objs)
		(format *state-output-stream* "begin-object~%")
		(with-slots (x y z) (slot-value obj 'pos)
			(format *state-output-stream* "pos ~a ~a ~a~%" x y z))
		(with-slots (w x y z) (slot-value obj 'ang-pos)
				(let ((len (magnitude (make-vector-3 x y z))))
					(print (list w x y z len))
					(if (/= 0 len) ; print in angle-axis form (in degrees, because that's what OpenGL uses *shudder*)
						(format *state-output-stream* "ang-pos ~a ~a ~a ~a~%"
							(/ (* (* 2 (acos w)) 180) *pi*)
							(/ x len)
							(/ y len)
							(/ z len))
						(format *state-output-stream* "ang-pos ~a ~a ~a ~a~%"
							0
							1
							0
							0))))
		(format *state-output-stream* "radius 1~%")
		(format *state-output-stream* "end-object~%"))
	(format *state-output-stream* "end-timestep~%"))

(defun main-loop (time-acceleration all-objs)
	(without-floating-point-underflow
		(let ((current-time (get-internal-real-time)) prev-time)
			(loop
				(setf prev-time current-time)
				(setf current-time (get-internal-real-time))
				(process-commands)
				(timestep all-objs (/ (- current-time prev-time) (/ internal-time-units-per-second time-acceleration)))
				(format *error-output* "dt: ~a~%" (+ 0.0 (/ (- current-time prev-time) (/ internal-time-units-per-second time-acceleration))))
				(print-timestep all-objs)))))
