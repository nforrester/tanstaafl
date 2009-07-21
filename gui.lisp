(let ((width 100) (height 100)) ; These two functions should share width and height. I like closures.
	(defun display ()
		(gl-viewport 0 0 width height) ; Draw the main scene
		(gl-matrix-mode *gl-projection*)
		(gl-load-identity)
		(glu-perspective 40 (/ width height) .1 1000)

		(gl-matrix-mode *gl-modelview*)
		(gl-load-identity)

		(gl-enable *gl-lighting*)
		(gl-enable *gl-light0*)
		(gl-enable *gl-cull-face*)

		; What I'm really achieving with these two calls is making everything look as though it was
		; from the viewpoint of the selected object. That's why they rotate everything backwards.
		(gl-rotate-quaternion-reverse (slot-value *focused-object* 'ang-pos))
		(gl-translate-vector-3 (mult -1 (slot-value *focused-object* 'pos)))

		(gl-clear (logior
			*gl-color-buffer-bit*
			*gl-depth-buffer-bit*))
		(loop for obj in *all-objs* do
			(draw obj))

		(gl-disable *gl-lighting*)
		(gl-disable *gl-light0*)
		(gl-disable *gl-cull-face*)

		; What I'm really achieving with these two calls is making everything look as though it was
		(loop for mfd in *all-mfds* do
			(draw-mfd mfd width height))
		(glut-swap-buffers))

	(defun reshape (window-width window-height)
		(setf width window-width)
		(setf height window-height)
		(glut-post-redisplay)))

(defgeneric draw (obj)
	(:documentation "Render something."))

(defmethod draw :around ((obj space-object)) ; to push a matrix on the stack and apply appropriate
                                             ; transformations to put the object in the right place,
					     ; and then pop the matrix off again afterwords.
	(gl-push-matrix)
	(gl-translate-vector-3 (slot-value obj 'pos))
	(gl-rotate-quaternion (slot-value obj 'ang-pos))
	(call-next-method)
	(gl-pop-matrix))

(defmethod draw ((obj space-object))
	(gl-rotated 90 0 1 0)
	(glut-solid-teapot 1))

(defvar *depressed-keys* ())

(defun check-depressed-keys (str)
	(< 0 (loop for key in *depressed-keys* counting (equal str key))))

(defun add-to-depressed-keys (key)
	(if (not (check-depressed-keys key))
		(push key *depressed-keys*)))

(defun remove-from-depressed-keys (key)
	(setf *depressed-keys* (delete key *depressed-keys*)))

(defun keyboard-down (key x y)
	(add-to-depressed-keys key))

(defun keyboard-up (key x y)
	(remove-from-depressed-keys key))

(defun mouse (button state x y)
	(print (list button state x y)))

(glut-init-window-position 0 0)
(glut-init-window-size 1000 1000)
(glut-init 0 "")
(glut-init-display-mode (logior *glut-double* *glut-rgb* *glut-depth*))
(glut-create-window "YINO")

(gl-clear-color 0 0 0 0)
(gl-shade-model *gl-smooth*)
(gl-enable *gl-depth-test*)

(gl-cull-face *gl-front*)

(glut-ignore-key-repeat 1)

(glut-display-func #'display)

(glut-reshape-func #'reshape)

(glut-keyboard-func #'keyboard-down)
(glut-keyboard-up-func #'keyboard-up)

(glut-special-func #'keyboard-down)
(glut-special-up-func #'keyboard-up)

(glut-mouse-func #'mouse)
