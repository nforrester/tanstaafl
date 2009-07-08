; YINO = YINO it's not orbiter
; By Neil Forrester

(load "physics.lisp")

(main-loop
	1
	(list
		(make-space-object
			:mass (/ 4.0 *G*)
			:pos (make-vector-3 1.0 0.0 0.0)
			:ang-vel (make-vector-3 0.5 2.0 1.0)
			:vel (make-vector-3 0.0 0.0 1.0))
		(make-space-object
			:mass (/ 4.0 *G*)
			:pos (make-vector-3 -1.0 0.0 0.0)
			:vel (make-vector-3 0.0 0.0 -1.0))))
