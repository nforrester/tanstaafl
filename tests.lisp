(defun report-result (result form)
	(if result
		(format t "pass ... ~a~%" *test-name*)
		(format t "FAIL ... ~a: ~a~%" *test-name* form))
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
	`(combine-results
		,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro deftest (name &rest tests)
	`(defun ,name ()
		(let ((*test-name* (append *test-name* (list ',name))))
			(check ,@tests))))

(defvar *test-name* nil)

; end unit test framework

(load "gsfs.lisp")

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

(deftest unit-tests
	(test-vectors))

(unit-tests)
