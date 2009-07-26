(ffi:default-foreign-language :stdc)

; To fetch values for constants out of header files when the constants are named things like
; GL_LIGHTING
; and name them things like
; *gl-lighting*

(defmacro fetch-constants (header-file &rest names)
	(let ((header-file header-file) (names names))
		(let (tw-stream i-stream o-stream (seperator "!@#$%^&*(seperator)*&^%$#@!") (c-names
				(loop for name in names collecting
					(list-strings-to-string
						(loop
							for word-in-name in (split-string #\- (second (split-string #\* (symbol-to-string name))))
							for word-count from 0
							collecting
								(let ((upcase (string-upcase word-in-name)))
									(if (= 0 word-count)
										upcase
										(concatenate 'string (list #\_) upcase))))))))
			`(progn ,@(let (defvars)
				; This is an abuse of the C preprocessor, using it to parse a header file and extract the values of constants defined in it.
				; The reason the Lisp forms slip through the preprocessor (mostly) untouched is that the preprocessor doesn't know the difference between C and Lisp.
				(multiple-value-setq (tw-stream i-stream o-stream) (ext:make-pipe-io-stream "gcc -E -"))
				(format tw-stream (concatenate 'string "#include <" header-file ">~%"))
				(format tw-stream seperator)
				(format tw-stream "~%")
				(loop
						for c-name in c-names
						for   name in   names
						do
					(format tw-stream "~a~%" c-name))
				(close o-stream)
				(let (line)
					(loop
						(setf line (read-line tw-stream))
						(if (equal seperator line)
							(return)))
					(setf defvars
						(loop for name in names collecting
							`(defvar ,name ,(read-from-string (concatenate 'string "#x" (second (split-string #\x (read-line tw-stream)))))))))
				(close i-stream)
				(close tw-stream)
				defvars)))))

; To automate the making of callouts for functions with names in the style of
; glutDisplayFunc
; and name them things like
; glut-display-func
; with some sugar wrapped around them to make numbers and things work right
; without explicit coersion/casting.
(defmacro gl-style-callout (name library &rest arguments)
	(let ((name name) (library library) (arguments arguments) (internal-name (gensym)))
		(let ((external-args (loop for i from 1 upto (list-length (mapcan #'(lambda (x) (if (eq :out (third x)) nil (list x))) arguments)) collecting (gensym))))
			`(progn
				(defun ,name ,external-args
					(,internal-name
						,@(loop
							for external-arg in external-args
							for arg in arguments
							collecting
								(cond
									((eq (second arg) 'ffi:int)
										`(round ,external-arg))
									((eq (second arg) 'ffi:single-float)
										`(coerce ,external-arg 'single-float))
									((eq (second arg) 'ffi:double-float)
										`(coerce ,external-arg 'double-float))
									(t
										external-arg)))))
				(ffi:def-call-out ,internal-name
					(:library ,library)
					(:arguments ,@arguments)
					;,(if (not (eq nil arguments)) `(:arguments ,@arguments))
					(:name
						,(list-strings-to-string
							(loop
								for word-in-name in (split-string #\- (symbol-to-string name))
								for word-count from 0
								collecting
									(if (= 0 word-count)
										(string-downcase word-in-name)
										(string-capitalize word-in-name))))))))))

(defmacro gl-style-callouts-single-library (library &rest callouts)
	(let ((library library))
		`(progn
			,@(loop for callout in callouts collecting
				`(gl-style-callout ,(first callout) ,library ,@(rest callout))))))

; And finally we define the bindings:

(gl-style-callouts-single-library "/usr/lib64/nvidia/libGL.so.1"
	(gl-clear-color (r ffi:double-float) (g ffi:double-float) (b ffi:double-float) (a ffi:double-float))
	(gl-shade-model (model ffi:uint))
	(gl-enable (option ffi:uint))
	(gl-disable (option ffi:uint))
	(gl-load-identity)
	(gl-matrix-mode (mode ffi:uint))
	(gl-viewport
		(x1 ffi:int)
		(y1 ffi:int)
		(x2 ffi:int)
		(y2 ffi:int))
	(gl-clear (buffer ffi:uint))
	(gl-cull-face (mode ffi:uint))
	(gl-push-matrix)
	(gl-pop-matrix)
	(gl-begin (mode ffi:uint))
	(gl-end)
	(gl-blend-func (srcfactor ffi:uint) (dstfactor ffi:uint))
	(gl-color3f
		(red ffi:single-float)
		(green ffi:single-float)
		(blue ffi:single-float))
	(gl-color4f
		(red ffi:single-float)
		(green ffi:single-float)
		(blue ffi:single-float)
		(alpha ffi:single-float))
	(gl-vertex2d
		(x ffi:double-float)
		(y ffi:double-float))
	(gl-vertex3d
		(x ffi:double-float)
		(y ffi:double-float)
		(z ffi:double-float))
	(gl-raster-pos2d
		(x ffi:double-float)
		(y ffi:double-float))
	(gl-raster-pos3d
		(x ffi:double-float)
		(y ffi:double-float)
		(z ffi:double-float))
	(gl-translated
		(x ffi:double-float)
		(y ffi:double-float)
		(z ffi:double-float))
	(gl-rotated
		(a ffi:double-float)
		(x ffi:double-float)
		(y ffi:double-float)
		(z ffi:double-float))
	(gl-get-doublev
		(pname ffi:uint)
		(params  (ffi:c-ptr (ffi:c-array ffi:double-float 16)) :out)))

(gl-style-callouts-single-library "/usr/lib64/libGLU.so"
	(glu-ortho2-d
		(left   ffi:double-float)
		(right  ffi:double-float)
		(bottom ffi:double-float)
		(top    ffi:double-float))
	(glu-perspective
		(fov          ffi:double-float)
		(aspect-ratio ffi:double-float)
		(near         ffi:double-float)
		(far          ffi:double-float))
	(glu-look-at
		(px ffi:double-float)
		(py ffi:double-float)
		(pz ffi:double-float)
		(tx ffi:double-float)
		(ty ffi:double-float)
		(tz ffi:double-float)
		(ux ffi:double-float)
		(uy ffi:double-float)
		(uz ffi:double-float))
	(glu-project
		(obj-x ffi:double-float)
		(obj-y ffi:double-float)
		(obj-z ffi:double-float)
		(model-matrix (ffi:c-array-ptr ffi:double-float))
		(proj-matrix  (ffi:c-array-ptr ffi:double-float))
		(viewport     (ffi:c-array-ptr ffi:int))
		(win-x (ffi:c-ptr ffi:double-float) :out)
		(win-y (ffi:c-ptr ffi:double-float) :out)
		(win-z (ffi:c-ptr ffi:double-float) :out)))

(gl-style-callouts-single-library "/usr/lib64/libglut.so"
	(glut-display-func        (func (ffi:c-function)))
	(glut-reshape-func        (func (ffi:c-function (:arguments (width ffi:int) (height ffi:int)))))
	(glut-keyboard-func       (func (ffi:c-function (:arguments (key ffi:character) (x ffi:int) (y ffi:int)))))
	(glut-keyboard-up-func    (func (ffi:c-function (:arguments (key ffi:character) (x ffi:int) (y ffi:int)))))
	(glut-special-func        (func (ffi:c-function (:arguments (key ffi:int) (x ffi:int) (y ffi:int)))))
	(glut-special-up-func     (func (ffi:c-function (:arguments (key ffi:int) (x ffi:int) (y ffi:int)))))
	(glut-mouse-func          (func (ffi:c-function (:arguments (button ffi:int) (state ffi:int) (x ffi:int) (y ffi:int)))))
	(glut-motion-func         (func (ffi:c-function (:arguments (x ffi:int) (y ffi:int)))))
	(glut-passive-motion-func (func (ffi:c-function (:arguments (x ffi:int) (y ffi:int)))))
	(glut-init-window-position
		(x ffi:int)
		(y ffi:int))
	(glut-init-window-size
		(x ffi:int)
		(y ffi:int))
	(glut-init-display-mode (mode ffi:uint))
	(glut-init (argc (ffi:c-ptr ffi:int)) (argv (ffi:c-ptr ffi:c-string)))
	(glut-create-window (title ffi:c-string))
	(glut-post-redisplay)
	(glut-swap-buffers)
	(glut-ignore-key-repeat (setting ffi:int))
	(glut-main-loop-event)
	(glut-main-loop)
	(glut-bitmap-character (font ffi:c-pointer) (chr ffi:character))
	(glut-solid-teapot (size ffi:double-float)))

(fetch-constants "GL/freeglut.h"
	*gl-projection*
	*gl-modelview*
	*gl-projection-matrix*
	*gl-modelview-matrix*
	*gl-color-buffer-bit*
	*gl-depth-buffer-bit*
	*glut-double*
	*glut-rgb*
	*glut-depth*
	*gl-smooth*
	*gl-depth-test*
	*gl-lighting*
	*gl-light0*
	*gl-lines*
	*gl-line-loop*
	*gl-quads*
	*gl-blend*
	*gl-src-alpha*
	*gl-one-minus-src-alpha*
	*gl-cull-face*
	*gl-front*
	*gl-back*
	*gl-front-and-back*
	*glut-down*
	*glut-up*)

(ffi:def-c-var *glut-bitmap-9-by-15-font* (:name "glutBitmap9By15") (:library "/usr/lib64/libglut.so") (:type ffi:c-pointer))
(defvar *glut-bitmap-9-by-15* (ffi:foreign-address *glut-bitmap-9-by-15-font*))

(defun gl-vertex-vector-2 (vec)
	(with-slots (x y) vec
		(gl-vertex2d x y)))

(defun gl-vertex-vector-3 (vec)
	(with-slots (x y z) vec
		(gl-vertex3d x y z)))

(defun gl-translate-vector-3 (vec)
	(with-slots (x y z) vec
		(gl-translated x y z)))

(defun gl-rotate-quaternion (quat)
	(with-slots (w x y z) quat
		(let ((len (magnitude (make-vector-3 x y z))))
			(if (/= 0 len) ; compute angle-axis form (in degrees, because that's what OpenGL uses *shudder*)
				(gl-rotated
					(/ (* (* 2 (acos w)) 180) *pi*)
					(/ x len)
					(/ y len)
					(/ z len))))))

(defun gl-rotate-quaternion-reverse (quat)
	(with-slots (w x y z) quat
		(let ((len (magnitude (make-vector-3 x y z))))
			(if (/= 0 len) ; compute angle-axis form (in degrees, because that's what OpenGL uses *shudder*)
				(gl-rotated
					(* -1 (/ (* (* 2 (acos w)) 180) *pi*))
					(/ x len)
					(/ y len)
					(/ z len))))))

(defmacro gl-begin-end (mode &rest forms)
	`(progn
		(gl-begin ,mode)
		,@forms
		(gl-end)))

(defmacro gl-enable-disable (mode &rest forms)
	`(progn
		(gl-enable ,mode)
		,@forms
		(gl-disable ,mode)))

(defun gl-color (color)
	(with-slots (red green blue alpha) color
		(gl-color4f red green blue alpha)))

(defun glu-project-vector-3-2 (pos model-matrix proj-matrix screen-size)
	(with-slots (x y z) pos
		(with-slots ((screen-width x) (screen-height y)) screen-size
			(multiple-value-bind (win-x win-y win-z)
					(glu-project
						x y z
						model-matrix
						proj-matrix
						(vector 0 0 screen-width screen-height))
				(make-vector-2 win-x win-y)))))
