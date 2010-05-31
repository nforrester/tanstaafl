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

;;;; READ BEFORE EDITING:
;;;; Please make sure that all generic functions that operate on the
;;;; class thruster also operate on thruster-group, so that thrusters
;;;; and thruster groups can be combined in interesting ways (groups
;;;; of groups, and other exotica).

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
     :documentation "The position of the thruster relative to the vessel (vector-3).")
   (power-level
     :initarg :power-level
     :initform 0
     :documentation "The power level of the thruster, between 0 and 1. The total of all the commanded values.")))

(defgeneric command (thruster power-level)
  (:documentation "Command the thruster to burn at the specified power level (between 0 and 1, unless you want to play games) during the next timestep that the burn function is executed (usually every timestep). The power level is added to the power levels specified by all previous command on the thruster since the last call to burn. The power level is then clamped between 0 and 1."))

(defmethod command ((thruster thruster) commanded-level)
  (with-slots (power-level) thruster
    (setf power-level (clamp 0 1 (+ power-level commanded-level)))))

(defgeneric burn (thruster)
  (:documentation "Burn the thruster at power-level for the timestep, and reset power-level to 0 (to ensure that a command isn't executed twice, effectively)."))

(defmethod burn ((thruster thruster))
  (with-slots (vessel max-thrust pos power-level) thruster
    (add-force-off-center vessel (mult power-level max-thrust) pos :frame :local)
    (setf power-level 0)))

(defgeneric compute-max-force (vessel thruster)
  (:documentation "Compute the maximum force that can be exerted on vessel by thruster."))

(defmethod compute-max-force ((target-vessel space-object) (thruster thruster))
  (with-slots (vessel max-thrust) thruster
    (if (eq target-vessel vessel)
      max-thrust
      0)))

(defgeneric compute-max-torque (vessel thruster)
  (:documentation "Compute the maximum torque that can be exerted on vessel by thruster."))

(defmethod compute-max-torque ((target-vessel space-object) (thruster thruster))
  (with-slots (vessel max-thrust pos) thruster
    (if (eq target-vessel vessel)
      (cross pos max-thrust)
      0)))

(defclass thruster-group ()
  ((thrusters
     :initarg :thrusters
     :initform ()
     :documentation "A list of the thrusters in the group.")))

(defmethod command ((thruster-group thruster-group) power-level)
  (dolist (thruster (slot-value thruster-group 'thrusters))
    (command thruster power-level)))

(defmethod burn ((thruster-group thruster-group))
  (dolist (thruster (slot-value thruster-group 'thrusters))
    (burn thruster)))

(defmethod compute-max-force ((target-vessel space-object) (thruster-group thruster-group))
  (let ((force (make-vector-3 0 0 0)))
    (dolist (thruster (slot-value thruster-group 'thrusters))
      (setf force (add force (compute-max-force target-vessel thruster))))
    force))

(defmethod compute-max-torque ((target-vessel space-object) (thruster-group thruster-group))
  (let ((torque (make-vector-3 0 0 0)))
    (dolist (thruster (slot-value thruster-group 'thrusters))
      (setf torque (add torque (compute-max-torque target-vessel thruster))))
    torque))
