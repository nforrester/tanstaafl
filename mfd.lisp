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

(defclass mfd (box-2d)
	((max-size
		:initarg :max-size
		:initform (make-vector-2 .4 .4)
		:documentation "The maximum size of the MFD, expressed as a fraction of the screen width. The MFD will be square, with sides as long as the shorter of the two specified values.")))

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
	(let ((size (compute-size mfd screen-size)) (sw-corner (compute-sw-corner mfd screen-size)))
		(with-slots (x y) sw-corner
			(gl-viewport x y size size)))
	(call-next-method))
