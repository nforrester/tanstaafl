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

(defvar *all-mfds* ())
(defvar *mfd-modes* ())

(make-instance 'text-button
               :anchor-point   (make-vector-2 1 0)
               :pos            (make-vector-2 1 0)
               :text           "New MFD"
               :text-color     (make-color 0 .8 .8 1)
               :click-function #'(lambda ()
                                   (make-instance 'menu
                                                  :anchor-point (make-vector-2 1 (/ -1 (+ 1 (length *mfd-modes*))))
                                                  :pos          (make-vector-2 1 0)
                                                  :items        *mfd-modes*
                                                  :selection-function #'(lambda (mode)
                                                                          (make-instance mode)))))

(defclass mfd (box-2d)
  ((max-size
     :initarg :max-size
     :initform (make-vector-2 .4 .4)
     :documentation "The maximum size of the MFD, expressed as a fraction of the screen width. The MFD will be square, with sides as long as the shorter of the two specified values.")
   (move-button)
   (resize-button)
   (close-button)
   (mode-button)
   (left-buttons
     :initform nil)
   (right-buttons
     :initform nil)
   (top-buttons
     :initform nil)
   (bottom-buttons
     :initform nil)))

(defmethod initialize-instance :after ((mfd mfd) &rest stuff)
  (setf *all-mfds* (cons mfd *all-mfds*))
  (setf (slot-value mfd 'move-button) (make-instance 'text-drag-button
                                                     :anchor-point   (make-vector-2 1 0)
                                                     :text           "#"
                                                     :text-color     (make-color 0 0 1 .8)
                                                     :drag-function  #'(lambda (drag-pos)
                                                                         (setf (slot-value mfd 'anchor-point) (make-vector-2 0 1))
                                                                         (setf (slot-value mfd 'pos) drag-pos))))
  (let (initial-drag-pos initial-max-size)
    (setf (slot-value mfd 'resize-button) (make-instance 'text-drag-button
                                                         :anchor-point   (make-vector-2 0 1)
                                                         :text           "+"
                                                         :text-color     (make-color 0 0 1 .8)
                                                         :click-function #'(lambda ()
                                                                             (setf initial-max-size (slot-value mfd 'max-size))
                                                                             (setf initial-drag-pos
                                                                                   (slot-value (slot-value mfd 'resize-button) 'pos)))
                                                         :drag-function  #'(lambda (drag-pos)
                                                                             (setf (slot-value mfd 'max-size) (add initial-max-size
                                                                                                                   (mult *reflect-y-matrix-2*
                                                                                                                         (sub drag-pos
                                                                                                                              initial-drag-pos))))))))
  (setf (slot-value mfd 'close-button) (make-instance 'text-button
                                                     :anchor-point   (make-vector-2 0 0)
                                                     :text           "X"
                                                     :text-color     (make-color .8 0 0 .8)
                                                     :click-function  #'(lambda ()
                                                                          (destroy mfd))))
  (setf (slot-value mfd 'mode-button) (make-instance 'text-button
                                                     :anchor-point   (make-vector-2 1 1)
                                                     :text           "M"
                                                     :text-color     (make-color 0 .8 0 .8)
                                                     :click-function  #'(lambda ()
                                                                          (print "HAHA MODE CHANGES AREN'T IMPLEMENTED YET SUCKER!!!")))))

(defgeneric compute-size (mfd screen-size))

(defmethod compute-size ((mfd mfd) screen-size)
  (with-slots ((screen-width x) (screen-height y)) screen-size
    (with-slots ((max-width x) (max-height y)) (slot-value mfd 'max-size)
      (min (* max-width screen-width) (* max-height screen-height)))))

(defmethod compute-sw-corner ((mfd mfd) screen-size)
  (with-slots (anchor-point pos) mfd
    (let ((size (compute-size mfd screen-size)))
      (sub (mult pos screen-size) (mult size anchor-point)))))

;;; I give unto thee a viewport. Do what you wish with it.
(defmethod draw-2d :around ((mfd mfd) screen-size)
  (with-slots (move-button resize-button close-button mode-button left-buttons right-buttons top-buttons bottom-buttons) mfd
    (let* ((size (compute-size mfd screen-size)) (sw-corner (compute-sw-corner mfd screen-size)) (buttons-per-side 8) (bps-1 (- buttons-per-side 1)))
      (flet ((position-button (b p ap)
               (setf (slot-value b 'anchor-point) ap)
               (setf (slot-value b 'pos)
                     (mult (pixels-to-fractional-matrix screen-size)
                           (add (mult (inverse (pixels-to-fractional-matrix (make-vector-2 size size)))
                                      p)
                                sw-corner)))))
        (position-button   move-button (make-vector-2 0 1) (make-vector-2 1 0))
        (position-button resize-button (make-vector-2 1 0) (make-vector-2 0 1))
        (position-button  close-button (make-vector-2 1 1) (make-vector-2 0 0))
        (position-button   mode-button (make-vector-2 0 0) (make-vector-2 1 1))
        (flet ((position-side-buttons (buttons side)
                 (loop for button in buttons for i do
                       (when (not (null button))
                         (cond
                           ((eq side 'l) (position-button button (make-vector-2 0 (- 1 (/ i bps-1))) (make-vector-2 1 (- 1 (/ i bps-1)))))
                           ((eq side 'r) (position-button button (make-vector-2 1 (- 1 (/ i bps-1))) (make-vector-2 0 (- 1 (/ i bps-1)))))
                           ((eq side 't) (position-button button (make-vector-2 (/ i bps-1) 1)       (make-vector-2 (/ i bps-1) 0)))
                           ((eq side 'b) (position-button button (make-vector-2 (/ i bps-1) 0)       (make-vector-2 (/ i bps-1) 1))))))))
          (position-side-buttons   left-buttons 'l)
          (position-side-buttons  right-buttons 'r)
          (position-side-buttons    top-buttons 't)
          (position-side-buttons bottom-buttons 'b)))
      (with-slots (x y) sw-corner
        (gl-viewport x y size size))))
  (call-next-method))

(defmethod destroy ((mfd mfd))
  (with-slots (move-button resize-button close-button mode-button left-buttons right-buttons top-buttons bottom-buttons) mfd
    (loop for button in (append (list move-button resize-button close-button mode-button) left-buttons right-buttons top-buttons bottom-buttons) do
          (when (not (null button))
            (destroy button))))
  (setf *all-mfds* (remove mfd *all-mfds*)))
