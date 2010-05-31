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

(defclass button (box-2d)
  ((size
     :initarg :size
     :initform (make-vector-2 40 40)
     :documentation "The size of the button in pixels as a vector-2.")
   (click-function
     :initarg :click-function
     :initform nil
     :documentation "The function called when the button is clicked")))

(defmethod initialize-instance :after ((button button) &rest stuff)
  (setf *all-buttons* (cons button *all-buttons*)))

(defmethod compute-sw-corner ((button button) screen-size)
  (with-slots (anchor-point pos size) button
    (sub (mult pos screen-size) (mult anchor-point size))))

;;; Make an appropriate viewport
(defmethod draw-2d :around ((button button) screen-size)
  (with-slots (size) button
    (let ((sw-corner (compute-sw-corner button screen-size)))
      (with-slots (x y) sw-corner
        (with-slots ((width x) (height y)) size
          (gl-viewport x y width height)))))
  (call-next-method))

(defmethod check-and-handle-click ((button button) mouse-button state click-pos screen-size)
  (with-slots (size click-function) button
    (let ((sw-corner (compute-sw-corner button screen-size)))
      (with-slots (x y) sw-corner
        (with-slots ((click-x x) (click-y y)) click-pos
          (with-slots ((width x) (height y)) size
            (if (and
                  (< x click-x (+ x  width))
                  (< y click-y (+ y height))
                  (= 0 mouse-button)
                  (= *glut-down* state)
                  click-function)
              (progn (funcall click-function)))))))))

(defclass background-button (button)
  ((background-color
     :initarg :background-color
     :initform (make-color 1 1 1 1))))

(defmethod draw-2d ((button background-button) screen-size)
  (gl-matrix-mode *gl-projection*)
  (gl-load-identity)
  (glu-ortho2-d 0 1 0 1)
  (gl-matrix-mode *gl-modelview*)
  (gl-load-identity)
  (gl-clear *gl-depth-buffer-bit*)
  (gl-color (slot-value button 'background-color))
  (gl-begin-end *gl-quads*
                (gl-vertex2d 0 0)
                (gl-vertex2d 0 1)
                (gl-vertex2d 1 1)
                (gl-vertex2d 1 0))
  (call-next-method))

(defclass text-button (button)
  ((text
    :initarg :text
    :initform ""
    :documentation "The text to display on the button. This determines the size of the button.")
   (text-color
    :initarg :text-color
    :initform (make-color 1 0 0 1))))

;;; Compute the size of the button from the text.
(defmethod draw-2d :around ((button text-button) screen-size)
  (with-slots (text size) button
    (with-slots (x y) size
      (setf x 0)
      (setf y 1)
      (let ((line-length 0))
        (dolist (c (concatenate 'list text))
          (if (not (equal #\Newline c))
            (progn
              (setf line-length (1+ line-length))
              (if (< x line-length)
                (setf x line-length)))
            (progn
              (setf line-length 0)
              (setf y (1+ y)))))
        (with-slots ((char-x x) (char-y y)) *char-size*
          (setf x (+ char-x (* x char-x)))
          (setf y (+ (* 0.5 char-y) (1- y) (* y char-y)))))))
  (call-next-method))

(defmethod draw-2d ((button text-button) screen-size)
  (gl-matrix-mode *gl-projection*)
  (gl-load-identity)
  (with-slots (x y) (slot-value button 'size)
    (glu-ortho2-d 0 x 0 y)
    (gl-matrix-mode *gl-modelview*)
    (gl-load-identity)
    (gl-color (slot-value button 'text-color))
    (with-slots ((char-x x) (char-y y)) *char-size*
      (gl-place-string (slot-value button 'text) (make-vector-2 (* 0.5 char-x) (- y (* 1.25 char-y)))))))

(defclass text-bg-button (background-button text-button) ())
