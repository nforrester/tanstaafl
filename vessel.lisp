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
			:rcs-pitch-up       (make-instance 'thruster-group)
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
			:main               (make-instance 'thruster-group)
			:retro              (make-instance 'thruster-group))
		:documentation "A plist of thruster groups.")
	(active-autopilot-modes
		:initarg :active-autopilot-modes
		:initform ()
		:documentation "A list of running autopilot modes. Each element in the list is a cons cell. (car list-element) is a symbol idendifying the autopilot function. (cdr list-element) is a function that takes one argument (the length of the timestep) and returns t or nil, meaning it's work is finished (and it should be removed from the list) or not, respectively. You probably want to construct each function with lambdas.")))

(defgeneric handle-key-presses (vessel)
	(:documentation "Identify keys that are depressed, and take appropriate action."))

(defmethod handle-key-presses ((vessel vessel))
	(with-slots (thruster-groups) vessel
		(keyboard-handler
			((#\8 #\/) (command (getf thruster-groups :rcs-lin-down) 1))
			((#\8)     (command (getf thruster-groups :rcs-pitch-down) 1))
			((#\2 #\/) (command (getf thruster-groups :rcs-lin-up) 1))
			((#\2)     (command (getf thruster-groups :rcs-pitch-up) 1))
			((#\4)     (command (getf thruster-groups :rcs-roll-port) 1))
			((#\6 #\/) (command (getf thruster-groups :rcs-lin-fore) 1))
			((#\6)     (command (getf thruster-groups :rcs-roll-starboard) 1))
			((#\9 #\/) (command (getf thruster-groups :rcs-lin-aft) 1))
			((#\1 #\/) (command (getf thruster-groups :rcs-lin-port) 1))
			((#\1)     (command (getf thruster-groups :rcs-yaw-port) 1))
			((#\3 #\/) (command (getf thruster-groups :rcs-lin-starboard) 1))
			((#\3)     (command (getf thruster-groups :rcs-yaw-starboard) 1))
			((#\+)     (command (getf thruster-groups :main) 1))
			((#\-)     (command (getf thruster-groups :retro) 1))
			((#\5)     (with-slots (active-autopilot-modes) vessel
					;; This is an autopilot mode that tries to set rotation rate to less than a small amount
					;; The reason it's stored as a closure around this function is that if the lambda expression
					;; was here, each instantiation wouldn't be eq to the next, which is necessary for having the
					;; 5 key toggle the autopilot on and off.
					(if (< 0 (loop for mode in active-autopilot-modes counting (eq 'kill-rotation (car mode))))
						(setf active-autopilot-modes
							(mapcan #'(lambda (mode) (if (eq 'kill-rotation (car mode)) nil (list mode))) active-autopilot-modes))
						(setf active-autopilot-modes (append
							(list (let ((zero-ang-vel (make-vector-3 0 0 0)))
								(cons 'kill-rotation #'(lambda (dt) (print (list dt (magnitude (slot-value vessel 'ang-vel))))
										(if (> .01 (magnitude (slot-value vessel 'ang-vel)))
											t
											(progn (hold-rotation-autopilot vessel dt zero-ang-vel) nil))))))
							active-autopilot-modes))))))))

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

				(main  (make-instance 'thruster :max-thrust (make-vector-3 0 0 -50) :pos (make-vector-3 0 0  .5) :vessel vessel))
				(retro (make-instance 'thruster :max-thrust (make-vector-3 0 0  50) :pos (make-vector-3 0 0 -.5) :vessel vessel)))
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

(defmethod burn ((obj vessel))
	(loop
			for thruster-group in (slot-value obj 'thruster-groups)
			for i do
		(if (oddp i) ; only the thruster groups, this is a plist.
			(burn thruster-group))))

(defgeneric call-autopilots (vessel dt)
	(:documentation "Calls the list of running autopilot modes, and removes modes if needed."))

(defmethod call-autopilots ((vessel vessel) dt)
	(with-slots (active-autopilot-modes) vessel
		(setf active-autopilot-modes (mapcan #'(lambda (mode return-value) (if return-value nil (list mode)))
			active-autopilot-modes
			(print (loop for autopilot-mode in active-autopilot-modes collecting (funcall (cdr autopilot-mode) dt)))))))

(defmethod compute-forces ((obj vessel) dt)
	(call-next-method)
	(handle-key-presses obj)
	(call-autopilots obj dt)
	(burn obj))
