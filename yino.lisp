; YINO = YINO it's not orbiter
; By Neil Forrester

(load "util.lisp")

(load "math.lisp")
(load "physics.lisp")

(load "char-set.lisp")
(load "gl-bindings.lisp")

(load "gui.lisp")

(load "mfd.lisp")
(load "button.lisp")
(load "hud-layer.lisp")

(load "test-mfd.lisp")
(load "test-button.lisp")
(load "test-hud.lisp")

(load "vessel.lisp")
(load "thruster.lisp")

(defvar *state-output-stream* t)
(defvar *command-input-stream* t)
(defvar *time-acceleration* 1)
(defvar *wait-for-command* nil)

(defun process-command-line-args ()
	(let (active-arg)
		(dolist (a *args*)
			(cond
				((or (string= a "-i") (string= a "--input")) (setf active-arg :input))
				((eq active-arg :input) (setf *command-input-stream* (open a)) (setf active-arg nil))

				((or (string= a "-o") (string= a "--output")) (setf active-arg :output))
				((eq active-arg :output) (setf *state-output-stream* (open a :direction :output)) (setf active-arg nil))

				((or (string= a "-t") (string= a "--time-acceleration")) (setf active-arg :time-acceleration))
				((eq active-arg :time-acceleration) (setf *time-acceleration* (read-from-string a)) (setf active-arg nil))

				((or (string= a "-w") (string= a "--wait-for-command")) (setf active-arg :wait-for-command))
				((eq active-arg :wait-for-command) (setf *wait-for-command* a) (setf active-arg nil))

				(t (format *error-output* "Unrecognized command line option: ~a" a))))))

(defun wait-for-command-if-needed ()
	(if (not (eq nil *wait-for-command*))
		(loop
			(if (equal *wait-for-command* (get-command))
				(return)
				(sleep .01)))))

; and here... we... go!

(process-command-line-args)

(wait-for-command-if-needed)

(defvar *all-objs*
	(cond
		(nil
			(list (make-instance 'vessel
					:pos (make-vector-3 0 0 0)
					:inertia-tensor (compute-inertia-tensor 1 1 1)
					:max-torque (make-vector-3 1 1 1)
					:ang-vel (make-vector-3 0 0 0))))
		(t
			(list
				(make-instance 'vessel
					:mass 1
					:pos (make-vector-3 0.0 0.0 -10.0)
					:inertia-tensor (compute-inertia-tensor 1 1 1)
					:max-torque (make-vector-3 1 1 1)
					:vel (make-vector-3 0.0 0.0 0.0))
				(make-instance 'space-object
					:mass 1
				;	:mass (/ 8.0 *G*)
					:pos (make-vector-3 0.0 0.0 0.0)
					:vel (make-vector-3 0.0 0.0 0.0))))))

(defvar *all-mfds*
	(list
		(make-instance 'test-mfd
			:anchor-point (make-vector-2 .2 .2)
			:pos (make-vector-2 .5 .5)
			:max-size (make-vector-2 .3 .3)
			:color (make-color 1 0 0 0.3))))

(defvar *all-buttons*
	(list
		(make-instance 'test-button
			:anchor-point (make-vector-2 0 0)
			:pos (make-vector-2 .9 .9)
			:size (make-vector-2 40 40)
			:color (make-color 0 1 0 0.5)
			:click-function #'(lambda () (print "click")))))

(defvar *all-hud-layers*
	(list
		(make-instance 'test-hud
			:color (make-color 0 0 1 0.8)
			:origin (first *all-objs*)
			:target (second *all-objs*))))

(setf *focused-object* (first *all-objs*))

(make-simple-thruster-setup (first *all-objs*))

(main-loop
	*time-acceleration*
	*all-objs*)
