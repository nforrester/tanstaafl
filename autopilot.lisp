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
		(with-slots (x y z) (rotate ang-vel (inverse ang-pos)) ; Angular velocity in the local frame of reference.
			(loop
					for thruster-key in (list
						:rcs-pitch-up
						:rcs-pitch-down
						:rcs-yaw-starboard
						:rcs-yaw-port
						:rcs-roll-starboard
						:rcs-roll-port)
					for thruster = (getf thruster-groups thruster-key) do
				(command thruster (clamp 0 1 (/ x (slot-value (mult dt (compute-max-torque vessel thruster)) 'x))))))))
