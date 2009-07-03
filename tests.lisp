(defvar *test-name* nil)

(defun report-result (result)
	(if result
		(format t "pass ... ~a~%" *test-name*)
		(format t "FAIL ... ~a~%" *test-name*))
	result)

(defmacro my-gensymer ((&rest names) &body forms)
	`(let ,(loop for name in names collect `(,name (gensym))) ,@forms))

(defmacro combine-results (&body forms)
	(my-gensymer (result)
		`(let ((,result t))
			,@(loop for f in forms collect
				`(unless ,f (setf ,result nil)))
			,result)))

(defmacro check (&body forms)
	`(report-result
		(combine-results
			,@forms)))

(defmacro deftest (name &rest tests)
	`(defun ,name ()
		(let ((*test-name* (append *test-name* (list ',name))))
			(check ,@tests))))

; end unit test framework

(load "physics.lisp")

(deftest test-add
	(let
			((one (make-instance 'vector-3))
			(two (make-instance 'vector-3)))
		(with-slots ((ox x) (oy y) (oz z)) one
		(with-slots ((tx x) (ty y) (tz z)) two
			(setf ox 4)
			(setf tx 7)
			(setf oy 2)
			(setf ty 6)
			(setf oz 10)
			(setf tz -3)

			(with-slots ((rx x) (ry y) (rz z)) (add one two)
			(and
				(= rx 11)
				(= ry 8)
				(= rz 7)))))))

(deftest test-mult
	(let ((vec (make-instance 'vector-3)))
		(with-slots (x y z) vec
			(setf x 2)
			(setf y 8)
			(setf z -4)

			(with-slots ((rx x) (ry y) (rz z)) (mult -2 vec)
			(and
				(= rx -4)
				(= ry -16)
				(= rz 8))))))

(deftest test-sub
	(let
			((one (make-instance 'vector-3))
			(two (make-instance 'vector-3)))
		(with-slots ((ox x) (oy y) (oz z)) one
		(with-slots ((tx x) (ty y) (tz z)) two
			(setf ox 4)
			(setf tx 7)
			(setf oy 2)
			(setf ty 6)
			(setf oz 10)
			(setf tz -3)

			(with-slots ((rx x) (ry y) (rz z)) (sub one two)
			(and
				(= rx -3)
				(= ry -4)
				(= rz 13)))))))

(deftest test-magnitude
	(let ((vec (make-instance 'vector-3)))
		(with-slots (x y z) vec
			(setf x 2)
			(setf y 8)
			(setf z -4))
		(= 916515 (floor (* 100000 (magnitude vec))))))

(deftest test-vectors
	(test-add)
	(test-mult)
	(test-sub)
	(test-magnitude))

(deftest test-gravity
	(let*
			((ob1 (make-instance 'space-object))
			(ob2 (make-instance 'space-object))
			(ob3 (make-instance 'space-object))
			(all-objs (list ob1 ob2 ob3)))
		(with-slots (mass pos) ob1
			(setf mass 50.0)
			(setf pos (make-instance 'vector-3))
			(with-slots (x y z) pos
				(setf x 23.0)
				(setf y 4.0)
				(setf z -16.0)))
		(with-slots (mass pos acc) ob2
			(setf mass 20.0)
			(setf pos (make-instance 'vector-3))
			(with-slots (x y z) pos
				(setf x 22.0)
				(setf y 40.0)
				(setf z 0.0))
			(setf acc (make-instance 'vector-3))
			(with-slots (x y z) acc
				(setf x 0.0)
				(setf y 0.0)
				(setf z 0.0)))
		(with-slots (mass pos) ob3
			(setf mass 75.0)
			(setf pos (make-instance 'vector-3))
			(with-slots (x y z) pos
				(setf x -10.0)
				(setf y -5.0)
				(setf z 12.0)))
		(compute-gravity ob2 all-objs)
		(with-slots (acc) ob2
			(with-slots (x y z) acc
				(and
					(= -8331170 (floor (* 1e19 x)))
					(= -3210857 (floor (* 1e18 y)))
					(= -5394134 (floor (* 1e19 z))))))))

(deftest test-integrate-acc-to-vel
	(let ((ob (make-instance 'space-object)))
		(with-slots (acc vel) ob
			(setf acc (make-instance 'vector-3))
			(with-slots (x y z) acc
				(setf x 6.0)
				(setf y 4.0)
				(setf z 1.5))
			(setf vel (make-instance 'vector-3))
			(with-slots (x y z) vel
				(setf x 2.5)
				(setf y 1.0)
				(setf z -8.7))
			(integrate-acc-to-vel ob 0.1)
			(with-slots (x y z) vel
				(and
					(=  310 (floor (* 100 x)))
					(=  140 (floor (* 100 y)))
					(= -855 (floor (* 100 z))))))))

(deftest test-integrate-vel-to-pos
	(let ((ob (make-instance 'space-object)))
		(with-slots (vel pos) ob
			(setf vel (make-instance 'vector-3))
			(with-slots (x y z) vel
				(setf x 6.0)
				(setf y 4.0)
				(setf z 1.5))
			(setf pos (make-instance 'vector-3))
			(with-slots (x y z) pos
				(setf x 2.5)
				(setf y 1.0)
				(setf z -8.7))
			(integrate-vel-to-pos ob 0.1)
			(with-slots (x y z) pos
				(and
					(=  310 (floor (* 100 x)))
					(=  140 (floor (* 100 y)))
					(= -855 (floor (* 100 z))))))))

(deftest test-physics
	(test-gravity)
	(test-integrate-acc-to-vel)
	(test-integrate-vel-to-pos))

(deftest test-all
	(test-vectors)
	(test-physics))

(deftest unit-tests
	(test-all))

(unit-tests)
