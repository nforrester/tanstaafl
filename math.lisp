(defvar *pi* 3.14159265358979323 (:documentation "Tasty pie"))

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
