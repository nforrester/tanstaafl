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

(defclass vessel (space-object)
	((max-torque
		:initarg :max-torque
		:initform (make-vector-3)
		:documentation "The maximum torque that the vessels attitude controls can exert along the three axes (a vector-3)")
	(thruster-groups
		:initarg :thruster-groups
		:initform (list
			:rcs-pitch-up      (make-instance 'thruster-group)
			:rcs-pitch-down     (make-instance 'thruster-group)
			:rcs-yaw-starboard  (make-instance 'thruster-group)
			:rcs-yaw-port       (make-instance 'thruster-group)
			:rcs-roll-starboard (make-instance 'thruster-group)
			:rcs-roll-port      (make-instance 'thruster-group)
			:rcs-lin-up         (make-instance 'thruster-group)
			:rcs-lin-down       (make-instance 'thruster-group)
			:rcs-lin-starboard  (make-instance 'thruster-group)
			:rcs-lin-port       (make-instance 'thruster-group)
			:rcs-lin-fore       (make-instance 'thruster-group)
			:rcs-lin-aft        (make-instance 'thruster-group)
			:main           (make-instance 'thruster-group)
			:retro          (make-instance 'thruster-group))
		:documentation "A plist of thruster groups.")))

(defgeneric handle-key-presses (vessel)
	(:documentation "use check-depressed-keys to identify keys that are depressed, and take appropriate action."))

(defmethod handle-key-presses ((vessel vessel))
	(with-slots (thruster-groups) vessel
		(cond
			((check-depressed-keys #\8)
				(if (check-depressed-keys #\/)
					(burn (getf thruster-groups :rcs-lin-down) 1)
					(burn (getf thruster-groups :rcs-pitch-down) 1)))
			((check-depressed-keys #\2)
				(if (check-depressed-keys #\/)
					(burn (getf thruster-groups :rcs-lin-up) 1)
					(burn (getf thruster-groups :rcs-pitch-up) 1)))
			((check-depressed-keys #\4)
				(burn (getf thruster-groups :rcs-roll-port) 1))
			((check-depressed-keys #\6)
				(if (check-depressed-keys #\/)
					(burn (getf thruster-groups :rcs-lin-fore) 1)
					(burn (getf thruster-groups :rcs-roll-starboard) 1)))
			((check-depressed-keys #\9)
				(if (check-depressed-keys #\/)
					(burn (getf thruster-groups :rcs-lin-aft) 1)))
			((check-depressed-keys #\1)
				(if (check-depressed-keys #\/)
					(burn (getf thruster-groups :rcs-lin-port) 1)
					(burn (getf thruster-groups :rcs-yaw-port) 1)))
			((check-depressed-keys #\3)
				(if (check-depressed-keys #\/)
					(burn (getf thruster-groups :rcs-lin-starboard) 1)
					(burn (getf thruster-groups :rcs-yaw-starboard) 1)))
			((check-depressed-keys #\+)
				(burn (getf thruster-groups :main) 1))
			((check-depressed-keys #\-)
				(burn (getf thruster-groups :retro) 1)))))

(defun make-simple-thruster-setup (vessel)
	(with-slots (thruster-groups) vessel
		(let
				; all the RCS thrusters are named by position-direction
				((fore-up       (make-instance 'thruster :max-thrust (make-vector-3 0  1 0) :pos (make-vector-3 0 0 -.5) :vessel vessel))
				(fore-down      (make-instance 'thruster :max-thrust (make-vector-3 0 -1 0) :pos (make-vector-3 0 0 -.5) :vessel vessel))
				(aft-up         (make-instance 'thruster :max-thrust (make-vector-3 0  1 0) :pos (make-vector-3 0 0  .5) :vessel vessel))
				(aft-down       (make-instance 'thruster :max-thrust (make-vector-3 0 -1 0) :pos (make-vector-3 0 0  .5) :vessel vessel))

				(fore-starboard (make-instance 'thruster :max-thrust (make-vector-3  1 0 0) :pos (make-vector-3 0 0 -.5) :vessel vessel))
				(fore-port      (make-instance 'thruster :max-thrust (make-vector-3 -1 0 0) :pos (make-vector-3 0 0 -.5) :vessel vessel))
				(aft-starboard  (make-instance 'thruster :max-thrust (make-vector-3  1 0 0) :pos (make-vector-3 0 0  .5) :vessel vessel))
				(aft-port       (make-instance 'thruster :max-thrust (make-vector-3 -1 0 0) :pos (make-vector-3 0 0  .5) :vessel vessel))

				(starboard-up   (make-instance 'thruster :max-thrust (make-vector-3 0  1 0) :pos (make-vector-3  .5 0 0) :vessel vessel))
				(starboard-down (make-instance 'thruster :max-thrust (make-vector-3 0 -1 0) :pos (make-vector-3  .5 0 0) :vessel vessel))
				(port-up        (make-instance 'thruster :max-thrust (make-vector-3 0  1 0) :pos (make-vector-3 -.5 0 0) :vessel vessel))
				(port-down      (make-instance 'thruster :max-thrust (make-vector-3 0 -1 0) :pos (make-vector-3 -.5 0 0) :vessel vessel))

				(starboard-fore (make-instance 'thruster :max-thrust (make-vector-3 0 0 -1) :pos (make-vector-3  .5 0 0) :vessel vessel))
				(starboard-aft  (make-instance 'thruster :max-thrust (make-vector-3 0 0  1) :pos (make-vector-3  .5 0 0) :vessel vessel))
				(port-fore      (make-instance 'thruster :max-thrust (make-vector-3 0 0 -1) :pos (make-vector-3 -.5 0 0) :vessel vessel))
				(port-aft       (make-instance 'thruster :max-thrust (make-vector-3 0 0  1) :pos (make-vector-3 -.5 0 0) :vessel vessel))

				(main (make-instance 'thruster :max-thrust (make-vector-3 0 0 5) :pos (make-vector-3 0 0 -.5) :vessel vessel))
				(retro (make-instance 'thruster :max-thrust (make-vector-3 0 0 -5) :pos (make-vector-3 0 0 .5) :vessel vessel)))
			(setf (slot-value (getf thruster-groups :rcs-pitch-up)       'thrusters) (list fore-up aft-down))
			(setf (slot-value (getf thruster-groups :rcs-pitch-down)     'thrusters) (list fore-down aft-up))
			(setf (slot-value (getf thruster-groups :rcs-yaw-starboard)  'thrusters) (list fore-starboard aft-port))
			(setf (slot-value (getf thruster-groups :rcs-yaw-port)       'thrusters) (list fore-port aft-starboard))
			(setf (slot-value (getf thruster-groups :rcs-roll-starboard) 'thrusters) (list starboard-down port-up))
			(setf (slot-value (getf thruster-groups :rcs-roll-port)      'thrusters) (list port-down starboard-up))
			(setf (slot-value (getf thruster-groups :rcs-lin-up)         'thrusters) (list fore-up aft-up))
			(setf (slot-value (getf thruster-groups :rcs-lin-down)       'thrusters) (list fore-down aft-down))
			(setf (slot-value (getf thruster-groups :rcs-lin-starboard)  'thrusters) (list fore-starboard aft-starboard))
			(setf (slot-value (getf thruster-groups :rcs-lin-port)       'thrusters) (list fore-port aft-port))
			(setf (slot-value (getf thruster-groups :rcs-lin-fore)       'thrusters) (list starboard-fore port-fore))
			(setf (slot-value (getf thruster-groups :rcs-lin-aft)        'thrusters) (list starboard-aft port-aft))
			(setf (slot-value (getf thruster-groups :main)               'thrusters) (list main))
			(setf (slot-value (getf thruster-groups :retro)              'thrusters) (list retro)))))

(defmethod compute-forces ((obj vessel) dt)
	(call-next-method)
	(handle-key-presses obj))
