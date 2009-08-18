; YINO = YINO it's not orbiter
; By Neil Forrester

(setf custom:*floating-point-contagion-ansi* t)

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
(load "relative-hud.lisp")

(load "vessel.lisp")
(load "thruster.lisp")

(load "vsop87.lisp")
(load "astronomical-bodies.lisp")

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

(defvar *epoch-time* 0d0)

(setf *earth* (make-instance 'vsop-planet
	:name "Earth"
	:mass 5.9742d24
	:radius 6.3781d6
	:vsop-interval 1
	:x-series-set *vsop-series-set-earth-x*
	:y-series-set *vsop-series-set-earth-y*
	:z-series-set *vsop-series-set-earth-z*))
(setf *tp1* (make-instance 'vessel
	:name "Teapot"
	:mass 1d0
	:pos (make-vector-3 1.4518590493763995d11 -3.4205701569802784d10 (+ 64000000d0 -2.2211526053121796d9))
	:inertia-tensor (compute-inertia-tensor 1d0 1d0 1d0)
	:radius 2d0
	:max-torque (make-vector-3 1d0 1d0 1d0)
	:vel (make-vector-3 0.0d0 0d0 -30000.0d0)))
(setf *tp2* (make-instance 'vessel
	:name "Teapot-2"
	:mass 1d0
	:pos (make-vector-3 1.4518590493763995d11 (+ 10d0 -3.4205701569802784d10) (+ 64000000d0 -2.2211526053121796d9))
	:inertia-tensor (compute-inertia-tensor 1d0 1d0 1d0)
	:radius 2d0
	:max-torque (make-vector-3 1d0 1d0 1d0)
	:vel (make-vector-3 0.0d0 0d0 -30000.0d0)))

(defvar *all-objs* (list *tp1* *tp2* *earth*))

(defvar *all-mfds*
	(list
		(make-instance 'test-mfd
			:anchor-point (make-vector-2 .2 .2)
			:pos (make-vector-2 .5 .5)
			:max-size (make-vector-2 .3 .3)
			:color (make-color 1 0 0 0.3))))

(defvar *all-buttons*
	(list
		(make-instance 'text-bg-button
			:anchor-point (make-vector-2 0 1)
			:text (format nil "Hello World!")
			:pos (make-vector-2 .2 .9)
			:background-color (make-color 0 1 0 0.5)
			:text-color       (make-color 1 0 0 1)
			:click-function #'(lambda () (print "click")))))

(defvar *all-hud-layers*
	(list
		(make-instance 'relative-hud
			:color (make-color 0 0 1 0.8)
			:origin (first *all-objs*)
			:target (second *all-objs*))
		(make-instance 'relative-hud
			:color (make-color 1 0.5 0 0.8)
			:origin (first *all-objs*)
			:target (third *all-objs*))))

(setf *focused-object* (first *all-objs*))

(make-simple-thruster-setup (first *all-objs*))

(main-loop
	*time-acceleration*
	*all-objs*)
