(defvar *last-modelview-matrix* nil)
(defvar *last-projection-matrix* nil)

(let ((screen-size (make-vector-2 100 100))) ; These functions should share screen-size. I like closures.
	(defun display ()
		(with-slots (x y) screen-size
			(gl-viewport 0 0 x y) ; Draw the main scene
			(gl-matrix-mode *gl-projection*)
			(gl-load-identity)
			(glu-perspective 40 (/ x y) .1 1000))

		(gl-matrix-mode *gl-modelview*)
		(gl-load-identity)

		(gl-enable *gl-lighting*)
		(gl-enable *gl-light0*)
		(gl-enable *gl-cull-face*)

		; What I'm really achieving with these two calls is making everything look as though it was
		; from the viewpoint of the selected object. That's why they rotate everything backwards.
		(gl-rotate-quaternion-reverse (slot-value *focused-object* 'ang-pos))
		(gl-translate-vector-3 (mult -1 (slot-value *focused-object* 'pos)))

		(setf *last-modelview-matrix* (gl-get-doublev *gl-modelview-matrix*))
		(setf *last-projection-matrix* (gl-get-doublev *gl-projection-matrix*))

		(gl-clear (logior
			*gl-color-buffer-bit*
			*gl-depth-buffer-bit*))
		(loop for obj in *all-objs* do
			(draw obj))

		(gl-disable *gl-lighting*)
		(gl-disable *gl-light0*)
		(gl-disable *gl-cull-face*)

		(loop for mfd in *all-mfds* do
			(draw-2d mfd screen-size))

		(loop for button in *all-buttons* do
			(draw-2d button screen-size))

		(loop for hud-layer in *all-hud-layers* do
			(draw-2d hud-layer screen-size))

		(glut-swap-buffers))

	(defun reshape (window-width window-height)
		(with-slots (x y) screen-size
			(setf x window-width)
			(setf y window-height))
		(glut-post-redisplay))

	(defun keyboard-down (key x y)
		(add-to-depressed-keys key))

	(defun keyboard-up (key x y)
		(remove-from-depressed-keys key))

	(defun mouse-click (mouse-button state click-x click-y)
		(with-slots (x y) screen-size
			(loop for button in *all-buttons* do
				(check-and-handle-click button mouse-button state (make-vector-2 click-x (- y click-y)) (make-vector-2 x y)))))

	(defun mouse-motion-down (x y)
		)

	(defun mouse-motion-up (x y)
		))

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

(defclass box-2d ()
	((anchor-point
		:initarg :anchor-point
		:initform (make-vector-2 0 0)
		:documentation "The anchor point of the box, in box coordinates, expressed as a fraction of the box height, as a vector-2")
	(pos
		:initarg :pos
		:initform (make-vector-2 .25 .25)
		:documentation "The position of the anchor point of the box, in window coordinates, expressed as a fraction of the screen width, as a vector-2")))

(defgeneric draw-2d (box screen-size)
	(:documentation "Draw something on the 2d overlay"))

(defgeneric check-and-handle-click (object mouse-button state click-pos screen-size)
        (:documentation "Given that a click (down or up) occurred at click-pos,
	test if object was affected and act accordingly.
	mouse-button shall be a number, state shall be t or nil for down or up"))

(defgeneric compute-sw-corner (object screen-size)
	(:documentation "Compute the lower left corner of object, and return a vector-2 in pixels."))

(defclass color ()
	((red
		:initarg :red
		:initform 1)
	(green
		:initarg :green
		:initform 1)
	(blue
		:initarg :blue
		:initform 1)
	(alpha
		:initarg :alpha
		:initform 1)))

(defun make-color (red green blue alpha)
	(make-instance 'color :red red :green green :blue blue :alpha alpha))

(defvar *depressed-keys* ())

(defun check-depressed-keys (str)
	(< 0 (loop for key in *depressed-keys* counting (equal str key))))

(defun add-to-depressed-keys (key)
	(if (not (check-depressed-keys key))
		(push key *depressed-keys*)))

(defun remove-from-depressed-keys (key)
	(setf *depressed-keys* (delete key *depressed-keys*)))

(glut-init-window-position 0 0)
(glut-init-window-size 1000 1000)
(glut-init 0 "")
(glut-init-display-mode (logior *glut-double* *glut-rgb* *glut-depth*))
(glut-create-window "YINO")

(gl-clear-color 0 0 0 0)
(gl-shade-model *gl-smooth*)
(gl-enable *gl-depth-test*)

(gl-blend-func *gl-src-alpha* *gl-one-minus-src-alpha*)
(gl-enable *gl-blend*)

(gl-cull-face *gl-front*)

(glut-ignore-key-repeat 1)

(glut-display-func #'display)
(glut-reshape-func #'reshape)

(glut-keyboard-func #'keyboard-down)
(glut-keyboard-up-func #'keyboard-up)
(glut-special-func #'keyboard-down)
(glut-special-up-func #'keyboard-up)

(glut-mouse-func #'mouse-click)
(glut-motion-func #'mouse-motion-down)
(glut-passive-motion-func #'mouse-motion-up)
