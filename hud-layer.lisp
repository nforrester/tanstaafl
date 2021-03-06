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

(defclass hud-layer ()
  ((color
     :initarg :color
     :initform (make-color 0 1 0 0.8))
   (buttons)
   (name
     :initarg :name
     :initform "HUD-LAYER")))

(let ((x 0)) (defun hud-button-counter () (setf x (+ x 1))))

(defvar *hud-modes* ())

(defvar *hud-button-grid* (make-instance 'box-2d-grid :anchor-point (make-vector-2 0 1) :pos (make-vector-2 0 1)))
(setf (slot-value *hud-button-grid* 'boxes) (list (list (make-instance 'text-button
                                                                       :text "New HUD Layer"
                                                                       :text-color (make-color 0 .8 .8 .8)
                                                                       :click-function #'(lambda ()
                                                                                           (make-instance 'menu
                                                                                                          :anchor-point (make-vector-2 0 (+ 1 (/ (+ 1 (length *all-hud-layers*)) (+ 1 (length *hud-modes*)))))
                                                                                                          :pos          (make-vector-2 0 1)
                                                                                                          :items        *hud-modes*
                                                                                                          :selection-function #'(lambda (mode)
                                                                                                                                  (make-instance mode)))))
                                                        1 0)))

(defmethod initialize-instance :after ((hud-layer hud-layer) &rest stuff)
  (setf *all-hud-layers* (cons hud-layer *all-hud-layers*))
  (let*
    ((close-button (make-instance 'text-button :text "X" :text-color (make-color 1 0 0 .5)))
     (label (make-instance 'text-button :text (slot-value hud-layer 'name) :text-color (slot-value hud-layer 'color)))
     (move-up (make-instance 'text-button :text "^" :text-color (slot-value hud-layer 'color)))
     (move-down (make-instance 'text-button :text "V" :text-color (slot-value hud-layer 'color)))
     (row (hud-button-counter))
     (close-button-record (list close-button 0 row))
     (label-record        (list label        1 row))
     (move-up-record      (list move-up      2 row))
     (move-down-record    (list move-down    3 row)))
    (setf (slot-value *hud-button-grid* 'boxes) (list* close-button-record label-record move-up-record move-down-record (slot-value *hud-button-grid* 'boxes)))
    (setf (slot-value hud-layer 'buttons) (list close-button label move-up move-down))
    (setf (slot-value close-button 'click-function) (lambda ()
						      (destroy hud-layer)))))

(defmethod draw-2d :around ((hud-layer hud-layer) screen-size)
  (with-slots (x y) screen-size
    (gl-viewport 0 0 x y)
    (gl-matrix-mode *gl-projection*)
    (gl-load-identity)
    (glu-ortho2-d 0 x 0 y))
  (gl-matrix-mode *gl-modelview*)
  (gl-load-identity)
  (gl-clear *gl-depth-buffer-bit*)
  (gl-color (slot-value hud-layer 'color))
  (call-next-method))

;;; Return value is a vector-2 of window coordinates.
(defun project-position-to-hud (pos model-matrix proj-matrix screen-size)
  (glu-project-vector-3-2 (sub pos (slot-value *focused-object* 'pos)) model-matrix proj-matrix screen-size))

(defmethod destroy ((hud-layer hud-layer))
  (with-slots (buttons) hud-layer
    (setf (slot-value *hud-button-grid* 'boxes) (remove-if #'(lambda (box)
							       (find (first box) buttons))
							   (slot-value *hud-button-grid* 'boxes)))
    (loop for button in buttons do (destroy button)))
  (setf *all-hud-layers* (remove hud-layer *all-hud-layers*)))
