; YINO = YINO it's not orbiter
; By Neil Forrester

(load "physics.lisp")

(defvar *state-output-stream* t)
(defvar *command-input-stream* t)
(defvar *time-acceleration* 1)
(defvar *wait-for-command* nil)

(defun process-command-line-args ()
	(let (active-arg)
		(dolist (a *args*)
			(cond
				((or (equal a "-i") (equal a "--input")) (setf active-arg :input))
				((eq active-arg :input) (setf *command-input-stream* (open a)) (setf active-arg nil))

				((or (equal a "-o") (equal a "--output")) (setf active-arg :output))
				((eq active-arg :output) (setf *state-output-stream* (open a :direction :output)) (setf active-arg nil))

				((or (equal a "-t") (equal a "--time-acceleration")) (setf active-arg :time-acceleration))
				((eq active-arg :time-acceleration) (setf *time-acceleration* (read-from-string a)) (setf active-arg nil))

				((or (equal a "-w") (equal a "--wait-for-command")) (setf active-arg :wait-for-command))
				((eq active-arg :wait-for-command) (setf *wait-for-command* a) (setf active-arg nil))

				(t (format *error-output* "Unrecognized command line option: ~a" a))))))

(defun wait-for-command-if-needed ()
	(if (not (eq nil *wait-for-command*))
		(loop
			(if (equal
					*wait-for-command*
					(read-line *command-input-stream*))
				(return)))))

; and here... we... go!

(process-command-line-args)

(wait-for-command-if-needed)

(main-loop
	*time-acceleration*
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
