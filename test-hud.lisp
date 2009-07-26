(defclass test-hud (hud-layer)
	((origin
		:initarg :origin
		:initform nil)
	(target
		:initarg :target
		:initform nil)))

(defmethod draw-2d ((hud test-hud) screen-size)
	(with-slots (origin target) hud
		(with-slots (model-matrix proj-matrix) target
			(let ((pos (glu-project-vector-3-2 (make-vector-3 0 0 0) model-matrix proj-matrix screen-size)))
				(gl-begin-end *gl-line-loop*
					(gl-vertex-vector-2 (add pos (make-vector-2  40  40)))
					(gl-vertex-vector-2 (add pos (make-vector-2 -40  40)))
					(gl-vertex-vector-2 (add pos (make-vector-2 -40 -40)))
					(gl-vertex-vector-2 (add pos (make-vector-2  40 -40))))))))
