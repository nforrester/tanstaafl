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

(defun project-position-to-hud (point-to-project view-point view-orientation screen-size field-of-view)
	(let
			((relative-point-to-project
				(sub point-to-project view-point))
			(center-of-projection-rectangle
				(rotate
					(make-vector-3
						0
						0
						(/ (slot-value screen-size 'y) (* 2 (tan (/ field-of-view 2)))))
					view-orientation)))
;		(add
;			(mult 0.5 screen-size)
			(with-slots (x y)
					(sub
						(mult
							(/
								(dot center-of-projection-rectangle center-of-projection-rectangle)
								(dot relative-point-to-project      center-of-projection-rectangle))
							relative-point-to-project)
						center-of-projection-rectangle)
				(make-vector-2 x y))));)
