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
     :documentation "This function gets called when a selection is made, with the selection's corresponding value as the argument")
   (cancel-option
     :initarg :cancel-option
     :initform t
     :documentation "When cancel-option is t, an extra option is added to the bottom of the menu, to dismiss the menu without calling the selection function.")))

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
                                      :text-color (make-color .9 .9 .9 .8)
                                      :background-color (make-color .2 .2 .2 .8))
                                    0
                                    (* -1 i))
                              boxes)))))
  (with-slots (boxes cancel-option) menu
    (when cancel-option
      (setf boxes (cons (list (make-instance
                                'text-bg-button
                                :click-function #'(lambda ()
                                                    (destroy menu))
                                :text "Cancel"
                                :text-color (make-color .9 0 0 .8)
                                :background-color (make-color .2 .2 .2 .8))
                              0
                              (* -1 (length items)))
                        boxes)))))

(defmethod destroy ((menu menu))
  (loop for box in (slot-value menu 'boxes) do
        (let ((button (first box)))
          (destroy button)))
  (call-next-method))
