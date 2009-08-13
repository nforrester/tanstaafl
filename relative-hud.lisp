(defclass relative-hud (hud-layer)
	((origin
		:initarg :origin
		:initform nil)
	(target
		:initarg :target
		:initform nil)))

(let ((hud-marker-size 40))
	(defmethod draw-2d ((hud relative-hud) screen-size)
		(with-slots (origin target) hud
			(multiple-value-bind (pos visible) (project-position-to-hud (slot-value target 'pos) *last-modelview-matrix* *last-projection-matrix* screen-size)
				(gl-place-string
					(format nil "~aP ~a" (if visible "+" "-") (slot-value target 'name))
					(add pos (make-vector-2 (* -1 hud-marker-size) (+ 10 hud-marker-size))))
				(gl-place-string
					(format nil "~3$" (magnitude (sub (slot-value target 'pos) (slot-value origin 'pos))))
					(add pos (make-vector-2 (* -1 hud-marker-size) (+ -15 (* -1 hud-marker-size)))))
				(gl-begin-end *gl-line-loop*
					(gl-vertex-vector-2 (add pos (make-vector-2       hud-marker-size        hud-marker-size)))
					(gl-vertex-vector-2 (add pos (make-vector-2 (* -1 hud-marker-size)       hud-marker-size)))
					(gl-vertex-vector-2 (add pos (make-vector-2 (* -1 hud-marker-size) (* -1 hud-marker-size))))
					(gl-vertex-vector-2 (add pos (make-vector-2       hud-marker-size  (* -1 hud-marker-size)))))
				(if (not visible)
					(gl-begin-end *gl-lines*
						(gl-vertex-vector-2 (add pos (make-vector-2 (* -1 hud-marker-size) 0)))
						(gl-vertex-vector-2 (add pos (make-vector-2       hud-marker-size  0))))))
			(multiple-value-bind (pos visible) (project-position-to-hud (add (slot-value origin 'pos) (sub (slot-value target 'vel) (slot-value origin 'vel))) *last-modelview-matrix* *last-projection-matrix* screen-size)
				(gl-place-string
					(format nil "~aV ~a" (if visible "+" "-") (slot-value target 'name))
					(add pos (make-vector-2 (* -1 hud-marker-size) (+ 10 hud-marker-size))))
				(gl-place-string
					(format nil "~3$" (magnitude (sub (slot-value target 'vel) (slot-value origin 'vel))))
					(add pos (make-vector-2 (* -1 hud-marker-size) (+ -15 (* -1 hud-marker-size)))))
				(gl-begin-end *gl-lines*
					(gl-vertex-vector-2 (add pos (make-vector-2       0                      hud-marker-size)))
					(gl-vertex-vector-2 (add pos (make-vector-2       0                (* -1 hud-marker-size))))
					(gl-vertex-vector-2 (add pos (make-vector-2 (* -1 hud-marker-size)       0)))
					(gl-vertex-vector-2 (add pos (make-vector-2       hud-marker-size        0))))
				(if visible
					(gl-begin-end *gl-line-loop*
						(gl-vertex-vector-2 (add pos (make-vector-2 (* -0.5 hud-marker-size)         0)))
						(gl-vertex-vector-2 (add pos (make-vector-2         0                (*  0.5 hud-marker-size))))
						(gl-vertex-vector-2 (add pos (make-vector-2 (*  0.5 hud-marker-size)         0)))
						(gl-vertex-vector-2 (add pos (make-vector-2         0                (* -0.5 hud-marker-size))))))))))
