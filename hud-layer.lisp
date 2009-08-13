(defclass hud-layer ()
	((color
		:initarg :color
		:initform (make-color 0 1 0 0.8))))

(defmethod draw-2d :around ((hud-layer hud-layer) screen-size)
	(with-slots (x y) screen-size
		(gl-viewport 0 0 x y)
		(gl-matrix-mode *gl-projection*)
		(gl-load-identity)
		(glu-ortho2-d
			0;(* -1 (/ x 2))
			x;(*  1 (/ x 2))
			0;(*  1 (/ y 2))
			y));(* -1 (/ y 2))))
	(gl-matrix-mode *gl-modelview*)
	(gl-load-identity)
	(gl-clear *gl-depth-buffer-bit*)
	(gl-color (slot-value hud-layer 'color))
	(call-next-method))

;;; Multiple return values. First is a vector-2 of window coordinates,
;;; second is t or nil, indicating whether the
;;; projected point is in front of or behind the camera.
(defun project-position-to-hud (pos model-matrix proj-matrix screen-size)
	(glu-project-vector-3-2 (sub pos (slot-value *focused-object* 'pos)) model-matrix proj-matrix screen-size))
