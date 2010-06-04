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

(defclass menu (box-2d-grid)
  ((selection-function
     :initarg :selection-function
     :initform nil
     :documentation "This function gets called when a selection is made, with the selection's corresponding value as the argument")))

(defmethod initialize-instance :after ((menu menu) &key items)
  (loop for item in items
        for i
        do
        (let ((text (first item)) (value (second item)))
          (with-slots (boxes selection-function) menu
            (setf boxes (cons (list (make-instance
                                      'text-bg-button
                                      :click-function #'(lambda ()
                                                          (destroy menu)
                                                          (funcall selection-function value))
                                      :text (format nil "~a" text)
                                      :background-color (make-color .8 .8 .8 .8))
                                    0
                                    i)
                              boxes))))))

(defmethod destroy ((menu menu))
  (loop for box in (slot-value menu 'boxes) do
        (let ((button (first box)))
          (destroy button)))
  (call-next-method))
