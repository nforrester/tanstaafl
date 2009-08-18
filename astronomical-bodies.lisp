(defclass spherical-body (space-object) ())

(defmethod draw ((obj spherical-body))
	(glut-solid-sphere (slot-value obj 'radius) 200 100))

;;; VSOP does things a little backwards (compared to normal numerical methods),
;;; computing the position, and letting you derive everything else from that by
;;; taking derivitives. In order to make things work out right with the rest of
;;; the physics engine, here's a breif explanation of how the normal functions
;;; map to what needs to happen with VSOP (all are hijacked by :around methods):
;;;
;;; compute-forces - evaluates the VSOP series sets, computes the position of the
;;;                  object, and stores it in vsop-pos. Then compares with the
;;;                  previous position (conveniently still found in pos), to find
;;;                  the velocity, and updates vsop87-vel appropriately. Then
;;;                  compares this result with the previous velocity (vel), and
;;;                  updates acc.
;;;
;;; integrate-acc-to-vel - Doesn't actually integrate anything, just copies
;;;                        vsop-vel to vel
;;;
;;; integrate-vel-to-pos - Doesn't actually integrate anything, just copies
;;;                        vsop-pos to pos

(defclass vsop-body (space-object)
	((vsop-pos
		:initform (make-vector-3)
		:initarg :vsop-pos
		:documentation "see above comment")
	(vsop-vel
		:initform (make-vector-3)
		:initarg :vsop-vel
		:documentation "see above comment")
	(vsop-interval
		:initform 10d0
		:initarg :vsop-interval
		:documentation "The interval for interpolation between vsop-reference-points")
	(vsop-ref-1
		:initform (make-instance 'vsop-reference-point :epoch 0)
		:initarg :vsop-ref-1
		:documentation "A vsop-reference-point")
	(vsop-ref-2
		:initform (make-instance 'vsop-reference-point :epoch -1)
		:initarg :vsop-ref-2
		:documentation "A vsop-reference-point")
	(x-series-set
		:initform ()
		:initarg :x-series-set)
	(y-series-set
		:initform ()
		:initarg :y-series-set)
	(z-series-set
		:initform ()
		:initarg :z-series-set)))

;;; This function hijacks the inherited behavior completely. See above.
(defmethod compute-forces :around ((obj vsop-body) dt)
	(with-slots
			(vsop-pos
			vsop-vel
			vsop-interval
			vsop-ref-1
			vsop-ref-2
			pos
			vel
			acc
			x-series-set
			y-series-set
			z-series-set)
				obj
		(multiple-value-setq (vsop-pos vsop-ref-1 vsop-ref-2) (vsop-compute-position
			*epoch-time*
			x-series-set
			y-series-set
			z-series-set vsop-interval vsop-ref-1 vsop-ref-2))
		(setf vsop-vel (mult (/ 1 dt) (sub vsop-pos pos)))
		(setf acc      (mult (/ 1 dt) (sub vsop-vel vel)))))

;;; This function hijacks the inherited behavior completely. See above.
(defmethod integrate-acc-to-vel :around ((obj vsop-body) dt)
	(with-slots (vel vsop-vel) obj
		(setf vel vsop-vel)))

;;; This function hijacks the inherited behavior completely. See above.
(defmethod integrate-vel-to-pos :around ((obj vsop-body) dt)
	(with-slots (pos vsop-pos) obj
		(setf pos vsop-pos)))

(defclass vsop-planet (spherical-body vsop-body) ())
