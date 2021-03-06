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

(defvar *focused-object* nil)
(defvar *current-modelview-matrix* nil)
(defvar *current-projection-matrix* nil)
(defvar *projection-matrix-for-object* (make-hash-table))

;;; Each depth interval is a list of objects that overlap in depth.
;;; The point is to get the maximum resolution out of the depth buffer.
;;; The best case scenario is that no objects overlap in depth,
;;; and we can use a seperate depth interval for each one.
(defclass depth-interval ()
  ((close-limit
     :initarg :close-limit)
   (far-limit
     :initarg :far-limit)
   (objects
     :initarg :objects)))

;;; Computes the distance from *focused-object* to object along the negative z-axis of *focused-object*
(defun depth-from-screen (object)
;  (distance *focused-object* object))
  (let ((view-vector (rotate (make-vector-3 0d0 0d0 -1d0) (slot-value *focused-object* 'ang-pos))))
    (dot view-vector (sub (slot-value object 'pos) (slot-value *focused-object* 'pos)))))

(let ((screen-size (make-vector-2 100 100))) ; These functions should share screen-size. I like closures.
  (defun display ()
    (with-slots (x y) screen-size
      (gl-viewport 0 0 x y)) ; Draw the main scene

    (gl-matrix-mode *gl-modelview*)
    (gl-load-identity)

    (gl-enable *gl-lighting*)
    (gl-enable *gl-light0*)
    (gl-enable *gl-cull-face*)

    (gl-enable *gl-texture-2d*)
    (gl-tex-envf *gl-texture-env* *gl-texture-env-mode* *gl-decal*)

    ;; What I'm really achieving with this call is making everything look as though it was
    ;; from the viewpoint of the selected object. That's why it rotates everything backwards.
    ;; The translation component is handled seperately (in an around method on draw) to give
    ;; the full precision of floats close to the camera, and let parts of the scene that are
    ;; further away have lower precision
    (gl-rotate-quaternion (inverse (slot-value *focused-object* 'ang-pos)))

    (gl-push-matrix)
    (gl-translate-vector-3 (sub (slot-value *sun* 'pos) (slot-value *focused-object* 'pos)))
    (gl-lightfv *gl-light0* *gl-position* (vector 0e0 0e0 0e0 1e0))
    (gl-pop-matrix)

    (setf *current-modelview-matrix* (gl-get-doublev *gl-modelview-matrix*))
    (setf *current-projection-matricies* ())

    (gl-clear (logior
                *gl-color-buffer-bit*
                *gl-depth-buffer-bit*))

    ;; Sort the objects by depth from the screen (distance from the viewer along the viewer's negative z-axis),
    ;; such that objects that are far in front of you come first and objects far behind are last,
    ;; and arrange them into depth intervals for drawing.
    (setf *all-objs* (sort *all-objs* #'(lambda (obj1 obj2) (> (depth-from-screen obj1) (depth-from-screen obj2)))))

    (let ((depth-intervals ()))
      (loop for prev-obj = obj for obj in *all-objs* do
            (let ((depth (depth-from-screen obj)))
              (if
                (or
                  (null (first depth-intervals))
                  (>
                    (- (depth-from-screen prev-obj) (slot-value prev-obj 'radius))
                    (+ depth (slot-value      obj 'radius))))
                (setf depth-intervals (append depth-intervals (list (make-instance 'depth-interval
                                                                                   :close-limit (- depth (slot-value obj 'radius))
                                                                                   :far-limit   (+ depth (slot-value obj 'radius))
                                                                                   :objects     (list obj)))))
                (progn
                  (setf (slot-value (car (last depth-intervals)) 'close-limit)
                        (min
                          (slot-value (car (last depth-intervals)) 'close-limit)
                          (- depth (slot-value obj 'radius))))
                  (setf (slot-value (car (last depth-intervals)) 'far-limit)
                        (max
                          (slot-value (car (last depth-intervals)) 'far-limit)
                          (+ depth (slot-value obj 'radius))))
                  (setf (slot-value (car (last depth-intervals)) 'objects)
                        (append (slot-value (car (last depth-intervals)) 'objects) (list obj)))))))
      (let ((ultimate-close-clipping-plane .1) (closest-far-clipping-plane .11))
        (loop for interval in depth-intervals do
              (with-slots (x y) screen-size
                (gl-matrix-mode *gl-projection*)
                (gl-load-identity)
                (glu-perspective 40 (/ x y) (max ultimate-close-clipping-plane (slot-value interval 'close-limit)) (max closest-far-clipping-plane (slot-value interval 'far-limit)))
                (setf *current-projection-matrix* (gl-get-doublev *gl-projection-matrix*))
                (gl-matrix-mode *gl-modelview*)
                (gl-clear *gl-depth-buffer-bit*))
              (loop for obj in (slot-value interval 'objects) do
                    (setf (gethash obj *projection-matrix-for-object*) *current-projection-matrix*)
                    (draw obj)))))

    (gl-disable *gl-texture-2d*)

    (gl-disable *gl-lighting*)
    (gl-disable *gl-light0*)
    (gl-disable *gl-cull-face*)

    (loop for grid in *all-box-2d-grids* do
          (draw-2d grid screen-size))

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

  (defun mouse-motion-down (move-x move-y)
    (with-slots (x y) screen-size
      (loop for button in *all-buttons* do
            (when (typep button 'drag-button)
              (check-and-handle-drag button (make-vector-2 move-x (- y move-y)) (make-vector-2 x y))))))

  (defun mouse-motion-up (move-x move-y)
    ))

(defun pixels-to-fractional-matrix (screen-size)
  (make-matrix-2-2 (/ 1.0 (slot-value screen-size 'x)) 0.0
                   0.0 (/ 1.0 (slot-value screen-size 'y))))

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

(defclass material ()
  ((ambient
     :initarg :ambient
     :initform (make-color 0.2 0.2 0.2 1.0))
   (diffuse
     :initarg :diffuse
     :initform (make-color 0.8 0.8 0.8 1.0))
   (specular
     :initarg :specular
     :initform (make-color 0.0 0.0 0.0 1.0))
   (emission
     :initarg :emission
     :initform (make-color 0.0 0.0 0.0 1.0))))

(defgeneric set-material (face material)
  (:documentation "Set the specified material for the specified face."))

(defmethod set-material (face (material material))
  (with-slots (ambient diffuse specular emission) material
    (gl-material-color face *gl-ambient* ambient)
    (gl-material-color face *gl-diffuse* diffuse)
    (gl-material-color face *gl-specular* specular)
    (gl-material-color face *gl-emission* emission)))

(defgeneric draw (obj)
  (:documentation "Render something."))

(defmethod draw :around ((obj space-object)) ; to push a matrix on the stack and apply appropriate
                                             ; transformations to put the object in the right place,
                                             ; and then pop the matrix off again afterwords.
  (gl-push-matrix)

  (gl-translate-vector-3 (sub (slot-value obj 'pos) (slot-value *focused-object* 'pos)))
  (gl-rotate-quaternion (slot-value obj 'ang-pos))

  (call-next-method)
  (gl-pop-matrix))

(let ((default-material (make-instance 'material)))
  (defmethod draw ((obj space-object))
    (gl-rotated 90 0 1 0)
    (set-material *gl-front-and-back* default-material)
    (glut-solid-teapot (/ (slot-value obj 'radius) 2))))

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

(defclass box-2d-grid (box-2d) ; a grid of boxes, which is itself a box. It works sort of like a grid in a frame, in Tk.
  ((boxes
     :initarg :boxes
     :initform ()
     :documentation "A list of lists of the form (box col row)")))

(defvar *all-box-2d-grids* ())

(defmethod initialize-instance :after ((grid box-2d-grid) &rest stuff)
  (setf *all-box-2d-grids* (cons grid *all-box-2d-grids*)))

(defmethod draw-2d ((grid box-2d-grid) screen-size)
  (let ((cells (make-hash-table :test 'equal)))
    (with-slots (boxes) grid
      (if boxes (progn
                  (let ((max-col (loop for box in boxes maximize (second box)))
                        (min-col (loop for box in boxes minimize (second box)))
                        (max-row (loop for box in boxes maximize (third box)))
                        (min-row (loop for box in boxes minimize (third box))))
                    (loop for row from min-row to max-row do
                          (loop for col from min-col to max-col do
                                (setf (gethash (cons row col) cells) nil)))
                    (loop for box in boxes do
                          (setf (gethash (cons (third box) (second box)) cells) (first box)))
                    (let* ((row-heights (loop for row from min-row to max-row collect
                                              (loop for col from min-col to max-col maximize
                                                    (if (gethash (cons row col) cells)
                                                      (slot-value (slot-value (gethash (cons row col) cells) 'size) 'y)
                                                      0))))
                           (col-widths (loop for col from min-col to max-col collect
                                             (loop for row from min-row to max-row maximize
                                                   (if (gethash (cons row col) cells)
                                                     (slot-value (slot-value (gethash (cons row col) cells) 'size) 'x)
                                                     0))))
                           (grid-sw-corner (with-slots (pos anchor-point) grid
                                             (sub (mult pos screen-size)
                                                  (mult anchor-point (make-vector-2
                                                                       (loop for width in col-widths sum width)
                                                                       (loop for height in row-heights sum height)))))))

                      (with-slots (x y) screen-size
                        (gl-viewport 0 0 x y)
                        (gl-matrix-mode *gl-projection*)
                        (gl-load-identity)
                        (glu-ortho2-d 0 x 0 y))
                      (gl-matrix-mode *gl-modelview*)
                      (gl-load-identity)
                      (gl-clear *gl-depth-buffer-bit*)
                      (gl-color (make-color 1 0 0 1))
                      (gl-begin-end *gl-line-loop*
                                    (gl-vertex-vector-2 (add grid-sw-corner (make-vector-2 0 0)))
                                    (gl-vertex-vector-2 (add grid-sw-corner (make-vector-2 (loop for width in col-widths sum width) 0)))
                                    (gl-vertex-vector-2 (add grid-sw-corner (make-vector-2 (loop for width in col-widths sum width) (loop for height in row-heights sum height))))
                                    (gl-vertex-vector-2 (add grid-sw-corner (make-vector-2 0 (loop for height in row-heights sum height)))))

                      (loop
                        for col from min-col to max-col
                        for width in col-widths
                        for x = (+ (if x x (slot-value grid-sw-corner 'x)) width)
                        do
                        (loop
                          for row from min-row to max-row
                          for height in row-heights
                          for y = (+ (if y y (slot-value grid-sw-corner 'y)) height)
                          do
                          (if (gethash (cons row col) cells)
                            (with-slots (anchor-point pos) (gethash (cons row col) cells)
                              (setf anchor-point (make-vector-2 1 1))
                              (setf pos (make-vector-2 (/ x (slot-value screen-size 'x)) (/ y (slot-value screen-size 'y)))))))))))))))

(defmethod destroy ((grid box-2d-grid))
  (setf *all-box-2d-grids* (remove grid *all-box-2d-grids*)))

(defvar *depressed-keys* ())

(defun check-depressed-keys (str)
  (< 0 (loop for key in *depressed-keys* counting (equal str key))))

(defun add-to-depressed-keys (key)
  (if (not (check-depressed-keys key))
    (push key *depressed-keys*)))

(defun remove-from-depressed-keys (key)
  (setf *depressed-keys* (delete key *depressed-keys*)))

(defmacro keyboard-handler (&rest key-mappings)
  `(cond ,@(loop for key-mapping in key-mappings collecting
                 `((and
                     ,@(loop for key in (first key-mapping) collecting
                             `(check-depressed-keys ,key)))
                   ,@(rest key-mapping)))))

(defun check-for-high-level-keyboard-commands ()
  (keyboard-handler
    ((#\e) (setf *time-acceleration* 1))
    ((#\r) (setf *time-acceleration* (/ *time-acceleration* 10)))
    ((#\t) (setf *time-acceleration* (* *time-acceleration* 10)))))

(glut-init-window-position 0 0)
(glut-init-window-size 1000 1000)
(glut-init 0 "")
(glut-init-display-mode (logior *glut-double* *glut-rgb* *glut-depth*))
(glut-create-window "TANSTAAFL")

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
