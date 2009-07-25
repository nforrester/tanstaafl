(defclass test-hud (hud-layer)
	((origin
		:initarg :origin
		:initform nil)
	(target
		:initarg :target
		:initform nil)))

(defmethod draw-2d ((hud test-hud) screen-size)
	(with-slots (origin target) hud
		(let ((pos (project-position-to-hud (slot-value target 'pos) (slot-value origin 'pos) (slot-value origin 'ang-pos) screen-size (/ 40 *radians-to-degrees*))))
			(gl-begin-end *gl-line-loop*
				(gl-vertex-vector-2 (add pos (make-vector-2  40  40)))
				(gl-vertex-vector-2 (add pos (make-vector-2 -40  40)))
				(gl-vertex-vector-2 (add pos (make-vector-2 -40 -40)))
				(gl-vertex-vector-2 (add pos (make-vector-2  40 -40)))))))
