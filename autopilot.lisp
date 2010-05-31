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

(defgeneric hold-rotation-autopilot (vessel dt target-ang-vel)
  (:documentation "Manipulates the rotation rcs thrusters of vessel for dt, in an attempt to keep it's rotational velocity equal to rotation-rate (in the local frame of reference)."))

(defmethod hold-rotation-autopilot ((vessel vessel) dt (target-ang-vel vector-3))
  (with-slots (ang-vel ang-pos thruster-groups) vessel
    (with-slots (x y z) (rotate (sub ang-vel target-ang-vel) (inverse ang-pos)) ; Angular velocity in the local frame of reference.
      (macrolet ((command-loop (axis thruster-list)
                               `(loop for thruster-key in ,thruster-list for thruster = (getf thruster-groups thruster-key) do
                                      (command thruster (clamp 0 1
							       (if (= 0 (slot-value (mult dt (compute-max-torque vessel thruster)) ,axis))
								 1
							         (/ (* -1 0.8 ,(eval axis)) (slot-value (mult dt (compute-max-torque vessel thruster)) ,axis))))))))
        (command-loop 'x (list :rcs-pitch-up :rcs-pitch-down))
        (command-loop 'y (list :rcs-yaw-starboard :rcs-yaw-port))
        (command-loop 'z (list :rcs-roll-starboard :rcs-roll-port))))))

(defgeneric hold-orientation-autopilot (vessel dt target-vector)
  (:documentation "Manipulates the rotation rcs thrusters of vessel for dt, in an attempt to keep it oriented at the target vector."))

(defmethod hold-orientation-autopilot ((vessel vessel) dt (target-vector vector-3))
  (let* ((vessel-orientation-vector (rotate (make-vector-3 0 0 -1) (slot-value vessel 'ang-pos)))
	 (angle-difference (acos (/ (dot target-vector vessel-orientation-vector)
				    (* (magnitude target-vector) (magnitude vessel-orientation-vector)))))
	 (ang-vel (mult (* 0.1 angle-difference)
			(normalize (cross vessel-orientation-vector target-vector)))))
    (when (< (* .003 pi) angle-difference)
      (hold-rotation-autopilot vessel dt ang-vel))))
