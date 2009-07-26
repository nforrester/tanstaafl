(defvar *G* 6.673e-11 (:documentation "Gravitational constant"))

(defclass space-object ()
	((mass
		:initarg :mass
		:initform 1.0
		:documentation "Mass measured in kg.")

	(pos
		:initarg :pos
		:initform (make-vector-3)
		:documentation "position measured in m, cartesian, as a vector-3")
	(vel
		:initarg :vel
		:initform (make-vector-3)
		:documentation "velocity measured in m/s, cartesian, as a vector-3")
	(acc
		:initarg :acc
		:initform (make-vector-3)
		:documentation "acceleration measured in m/s/s, cartesian, as a vector-3")

	(inertia-tensor
		:initarg :inertia-tensor
		:initform (compute-inertia-tensor 1 1 1)
		:documentation "inertia tensor, as a matrix-3-3")
	(ang-pos
		:initarg :ang-pos
		:initform (make-quaternion)
		:documentation "angular position, as a quaternion")
	(ang-vel
		:initarg :ang-vel
		:initform (make-vector-3)
		:documentation "angular velocity in radians/s, as a vector-3")
	(ang-acc
		:initarg :ang-acc
		:initform (make-vector-3)
		:documentation "angular acceleration in radians/s/s, as a vector-3")
	(model-matrix
		:initform nil
		:documentation "Stores the last modelview matrix used to draw this object")
	(proj-matrix
		:initform nil
		:documentation "Stores the last projection matrix used to draw this object")))

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
	(with-slots (inertia-tensor ang-acc) obj
		(setf ang-acc (add ang-acc (mult (inverse inertia-tensor)
			(if (eq frame :global)
				torque
				(rotate torque (slot-value obj 'ang-pos))))))))

(defgeneric add-force-off-center (obj force pos &key)
	(:documentation "apply a force to obj, compute the associated torque, and update acc and ang-acc accordingly"))

(defmethod add-force-off-center ((obj space-object) (force vector-3) (pos vector-3) &key (frame :local))
	(add-force obj force :frame frame)
	(add-torque obj (cross pos force) :frame frame))

(defgeneric compute-forces (obj all-objs)
	(:documentation "compute the forces on obj, and update acc and ang-acc"))

(defmethod compute-forces :before ((obj space-object) all-objs)
	(setf (slot-value obj     'acc) (make-vector-3 0 0 0))
	(setf (slot-value obj 'ang-acc) (make-vector-3 0 0 0)))

(defmethod compute-forces ((obj space-object) all-objs)
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
			(setf ang-pos (add ang-pos (mult dt (mult 0.5 (mult (make-quaternion 0 x y z) ang-pos)))))
			(normalize ang-pos))))

(defun timestep (all-objs dt)
	(loop for obj in all-objs do
		(compute-forces obj all-objs))
	(loop for obj in all-objs do
		(integrate-acc-to-vel obj dt)
		(integrate-ang-acc-to-ang-vel obj dt)
		(integrate-vel-to-pos obj dt)
		(integrate-ang-vel-to-ang-pos obj dt)))

(defun print-timestep (all-objs)
;	(format *error-output* "dt: ~a~%" (+ 0.0 (/ (- current-time prev-time) (/ internal-time-units-per-second time-acceleration))))
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
				(timestep all-objs (/ (- current-time prev-time) (/ internal-time-units-per-second time-acceleration)))
				(glut-post-redisplay)
				(glut-main-loop-event)))))
