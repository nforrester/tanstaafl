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
		(let ((external-args (loop for i from 1 upto (list-length arguments) collecting (gensym))))
			`(progn
				(defun ,name ,external-args
					(,internal-name
						,@(loop
							for external-arg in external-args
							for arg in arguments
							collecting
								(cond
									((eq 'ffi:single-float (second arg))
										`(coerce ,external-arg 'single-float))
									((eq 'ffi:double-float (second arg))
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
	(gl-load-identity)
	(gl-matrix-mode (mode ffi:uint))
	(gl-viewport
		(x1 ffi:int)
		(y1 ffi:int)
		(x2 ffi:int)
		(y2 ffi:int))
	(gl-clear (buffer ffi:uint))
	(gl-push-matrix)
	(gl-pop-matrix)
	(gl-translated
		(x ffi:double-float)
		(y ffi:double-float)
		(z ffi:double-float))
	(gl-rotated
		(a ffi:double-float)
		(x ffi:double-float)
		(y ffi:double-float)
		(z ffi:double-float)))

(gl-style-callouts-single-library "/usr/lib64/libGLU.so"
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
		(uz ffi:double-float)))

(gl-style-callouts-single-library "/usr/lib64/libglut.so"
	(glut-display-func     (func (ffi:c-function)))
	(glut-keyboard-func    (func (ffi:c-function (:arguments (key ffi:character) (x ffi:int) (y ffi:int)))))
	(glut-keyboard-up-func (func (ffi:c-function (:arguments (key ffi:character) (x ffi:int) (y ffi:int)))))
	(glut-special-func     (func (ffi:c-function (:arguments (key ffi:int) (x ffi:int) (y ffi:int)))))
	(glut-special-up-func  (func (ffi:c-function (:arguments (key ffi:int) (x ffi:int) (y ffi:int)))))
	(glut-mouse-func       (func (ffi:c-function (:arguments (button ffi:int) (state ffi:int) (x ffi:int) (y ffi:int)))))
	(glut-reshape-func     (func (ffi:c-function (:arguments (width ffi:int) (height ffi:int)))))
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
	(glut-solid-teapot (size ffi:double-float)))

(fetch-constants "GL/freeglut.h"
	*gl-projection*
	*gl-modelview*
	*gl-color-buffer-bit*
	*gl-depth-buffer-bit*
	*glut-double*
	*glut-rgb*
	*glut-depth*
	*gl-smooth*
	*gl-depth-test*
	*gl-lighting*
	*gl-light0*)

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
