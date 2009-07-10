; YINO = YINO it's not orbiter
; By Neil Forrester

(load "math.lisp")
(load "physics.lisp")
(load "commands.lisp")
(load "vessel.lisp")

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

(main-loop
	*time-acceleration*
	(list (make-vessel
			:pos (make-vector-3 0 0 0)
			:inertia-tensor (compute-inertia-tensor 1 1 1)
			:max-torque (make-vector-3 1 1 1)
			:ang-vel (make-vector-3 0 0 0))))
;	(list
;		(make-space-object
;			:mass (/ 4.0 *G*)
;			:pos (make-vector-3 1.0 0.0 0.0)
;			:vel (make-vector-3 0.0 0.0 1.0))
;		(make-space-object
;			:mass (/ 4.0 *G*)
;			:pos (make-vector-3 -1.0 0.0 0.0)
;			:vel (make-vector-3 0.0 0.0 -1.0))))
