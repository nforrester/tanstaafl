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

(defclass thruster ()
	((vessel
		:initarg :vessel
		:initform nil
		:documentation "The vessel the thruster is attached to.")
	(max-thrust
		:initarg :max-thrust
		:initform (list 0.0 0.0 1.0)
		:documentation "The maximum thrust the thruster can produce (vector-3).")
	(pos
		:initarg :pos
		:initform (list 0.0 0.0 0.0)
		:documentation "The position of the thruster relative to the vessel (vector-3).")))

(defgeneric burn (thruster power-level)
	(:documentation "Burn the thruster at the specified power level (between 0 and 1, unless you want to play games)."))

(defmethod burn ((thruster thruster) power-level)
	(with-slots (vessel max-thrust pos) thruster
		(add-force-off-center vessel (mult power-level max-thrust) pos :frame :local)))

(defclass thruster-group ()
	((thrusters
		:initarg :thrusters
		:initform ()
		:documentation "A list of the thrusters in the group.")))

(defmethod burn ((thruster-group thruster-group) power-level)
	(dolist (thruster (slot-value thruster-group 'thrusters))
		(burn thruster power-level)))
