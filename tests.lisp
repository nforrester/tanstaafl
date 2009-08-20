; Tanstaafl - A free space flight simulator
; Copyright (C) 2009  Neil Forrester
; 
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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

(load "math.lisp")
(load "physics.lisp")

(defmacro check-xyz (vec x y z)
	`(with-slots (x y z) ,vec
		(and
			(= x ,x)
			(= y ,y)
			(= z ,z))))

(defmacro check-wxyz (quat w x y z)
	`(with-slots (w x y z) ,quat
		(and
			(= w ,w)
			(= x ,x)
			(= y ,y)
			(= z ,z))))

(deftest test-make-vector-3
	(let
			((one (make-vector-3))
			(two (make-vector-3 2.0 3.0 4)))
		(and
			(check-xyz one 0.0 0.0 0.0)
			(check-xyz two 2.0 3.0 4))))

(deftest test-add
	(let
			((one (make-vector-3 4 2 10))
			(two (make-vector-3 7 6 -3)))
		(check-xyz (add one two) 11 8 7)))

(deftest test-mult
	(let ((vec (make-vector-3 2 8 -4)))
		(check-xyz (mult -2 vec) -4 -16 8)))

(deftest test-sub
	(let
			((one (make-vector-3 4 2 10))
			(two (make-vector-3 7 6 -3)))
		(check-xyz (sub one two) -3 -4 13)))

(deftest test-magnitude
	(let ((vec (make-vector-3 2 8 -4)))
		(= 916515 (floor (* 100000 (magnitude vec))))))

(deftest test-vectors
	(test-make-vector-3)
	(test-add)
	(test-mult)
	(test-sub)
	(test-magnitude))

(deftest test-make-quaternion
	(check-wxyz (make-quaternion 23 42 68 -5)
		23
		42
		68
		-5))

(deftest test-qqmult
	(check-wxyz
		(mult
			(make-quaternion 23 42  68 -5)
			(make-quaternion  9  3 -42  7))
		 2972
		  713
		 -663
		-1852))

(deftest test-sqmult
	(check-wxyz
		(mult
			42
			(make-quaternion 9 3 -42 7))
		  378
		  126
		-1764
		  294))

(deftest test-qmult
	 (test-qqmult)
	 (test-sqmult))

(deftest test-quaternions
	(test-make-quaternion)
	(test-qmult))

(deftest test-make-space-object
	(let
			((one (make-instance 'space-object))
			(two (make-instance 'space-object
				:mass 4
				:pos (make-vector-3 4.0 2.0 3)
				:acc (make-vector-3 14.0 -2 3.3)
				:vel (make-vector-3 -40.0 29 -3))))
		(and
			(with-slots (mass pos acc vel) one
				(and
					(= mass 1.0)
					(check-xyz pos 0.0 0.0 0.0)
					(check-xyz vel 0.0 0.0 0.0)
					(check-xyz acc 0.0 0.0 0.0)))
			(with-slots (mass pos acc vel) two
				(and
					(= mass 4)
					(check-xyz pos 4.0 2.0 3)
					(check-xyz vel -40.0 29 -3)
					(check-xyz acc 14.0 -2 3.3))))))

(deftest test-gravity
	(let*
			((ob1 (make-instance 'space-object
				:mass 50.0
				:pos (make-vector-3 23.0 4.0 -16.0)))
			(ob2 (make-instance 'space-object
				:mass 20.0
				:pos (make-vector-3 22.0 40.0 0.0)
				:acc (make-vector-3 0.0 0.0 0.0)))
			(ob3 (make-instance 'space-object
				:mass 75.0
				:pos (make-vector-3 -10.0 -5.0 12.0)))
			(all-objs (list ob1 ob2 ob3)))
		(compute-gravity ob2 all-objs)
		(with-slots (acc) ob2
			(with-slots (x y z) acc
				(and
					(= -8331169 (floor (* 1e19 x)))
					(= -3210857 (floor (* 1e18 y)))
					(= -5394134 (floor (* 1e19 z))))))))

(deftest test-integrate-acc-to-vel
	(let ((ob (make-instance 'space-object
			:acc (make-vector-3 6.0 4.0 1.5)
			:vel (make-vector-3 2.5 1.0 -8.7))))
		(with-slots (vel) ob
			(integrate-acc-to-vel ob 0.1)
			(with-slots (x y z) vel
				(and
					(=  310 (floor (* 100 x)))
					(=  140 (floor (* 100 y)))
					(= -855 (floor (* 100 z))))))))

(deftest test-integrate-vel-to-pos
	(let ((ob (make-instance 'space-object
			:vel (make-vector-3 6.0 4.0 1.5)
			:pos (make-vector-3 2.5 1.0 -8.7))))
		(with-slots (pos) ob
			(integrate-vel-to-pos ob 0.1)
			(with-slots (x y z) pos
				(and
					(=  310 (floor (* 100 x)))
					(=  140 (floor (* 100 y)))
					(= -855 (floor (* 100 z))))))))

(deftest test-physics
	(test-make-space-object)
	(test-gravity)
	(test-integrate-acc-to-vel)
	(test-integrate-vel-to-pos))

(deftest test-all
	(test-vectors)
	(test-quaternions)
	(test-physics))

(deftest unit-tests
	(test-all))

(unit-tests)
