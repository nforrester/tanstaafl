(defclass relative-hud (hud-layer)
	((origin
		:initarg :origin
		:initform nil)
	(target
		:initarg :target
		:initform nil)))

(defmethod draw-2d ((hud relative-hud) screen-size)
	(with-slots (origin target) hud
		(multiple-value-bind (pos visible) (glu-project-vector-3-2 (slot-value target 'pos) *last-modelview-matrix* *last-projection-matrix* screen-size)
			(gl-place-string
				(format nil "~3$" (magnitude (sub (slot-value target 'pos) (slot-value origin 'pos))))
				(add pos (make-vector-2  -40  50)))
			(gl-begin-end *gl-line-loop*
				(gl-vertex-vector-2 (add pos (make-vector-2  40  40)))
				(gl-vertex-vector-2 (add pos (make-vector-2 -40  40)))
				(gl-vertex-vector-2 (add pos (make-vector-2 -40 -40)))
				(gl-vertex-vector-2 (add pos (make-vector-2  40 -40))))
			(if (not visible)
				(gl-begin-end *gl-lines*
					(gl-vertex-vector-2 (add pos (make-vector-2 -40 0)))
					(gl-vertex-vector-2 (add pos (make-vector-2  40 0))))))
		(multiple-value-bind (pos visible) (glu-project-vector-3-2 (add (slot-value origin 'pos) (sub (slot-value target 'vel) (slot-value origin 'vel))) *last-modelview-matrix* *last-projection-matrix* screen-size)
			(gl-place-string
				(format nil "~3$" (magnitude (sub (slot-value target 'vel) (slot-value origin 'vel))))
				(add pos (make-vector-2  -40  50)))
			(gl-begin-end *gl-lines*
				(gl-vertex-vector-2 (add pos (make-vector-2   0  40)))
				(gl-vertex-vector-2 (add pos (make-vector-2   0 -40)))
				(gl-vertex-vector-2 (add pos (make-vector-2 -40   0)))
				(gl-vertex-vector-2 (add pos (make-vector-2  40   0))))
			(if visible
				(gl-begin-end *gl-line-loop*
					(gl-vertex-vector-2 (add pos (make-vector-2 -20   0)))
					(gl-vertex-vector-2 (add pos (make-vector-2   0  20)))
					(gl-vertex-vector-2 (add pos (make-vector-2  20   0)))
					(gl-vertex-vector-2 (add pos (make-vector-2   0 -20))))))))
