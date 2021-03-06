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

(defvar *G* 6.673d-11 (:documentation "Gravitational constant"))
(defvar *m-per-au* 149598000000d0)

(defclass space-object ()
  ((name
     :initarg :name
     :initform ""
     :documentation "The name of the object.")
   (mass
     :initarg :mass
     :initform 1.0
     :documentation "Mass measured in kg.")

   (pos
     :initarg :pos
     :initform (make-vector-3)
     :documentation "position measured in m, cartesian, as a vector-3")
   (vel
     :initarg :vel
     :initform (make-vector-3)
     :documentation "velocity measured in m/s, cartesian, as a vector-3")
   (acc
     :initarg :acc
     :initform (make-vector-3)
     :documentation "acceleration measured in m/s/s, cartesian, as a vector-3")

   (inertia-tensor
     :initarg :inertia-tensor
     :initform (compute-inertia-tensor 1 1 1)
     :documentation "inertia tensor, as a matrix-3-3")
   (ang-pos
     :initarg :ang-pos
     :initform (make-quaternion)
     :documentation "angular position, as a quaternion")
   (ang-vel
     :initarg :ang-vel
     :initform (make-vector-3)
     :documentation "angular velocity in radians/s, as a vector-3")
   (ang-acc
     :initarg :ang-acc
     :initform (make-vector-3)
     :documentation "angular acceleration in radians/s/s, as a vector-3")
   (radius
     :initarg :radius
     :initform 1
     :documentation "The radius of the space object, used for determining whether objects overlap in depth, and thus need to be in the same depth layer for drawing. spherical-bodies also use this as their literal radius.")))

(defmethod distance ((o1 space-object) (o2 space-object))
  (distance
    (slot-value o1 'pos)
    (slot-value o2 'pos)))

(defun compute-inertia-tensor (x y z)
  (make-matrix-3-3
    x 0 0
    0 y 0
    0 0 z))

(defgeneric compute-gravity (obj)
  (:documentation "compute gravity on obj due to *all-objs*."))

(defmethod compute-gravity ((obj space-object))
  (loop for other-obj in *all-objs* do
	(unless (eq obj other-obj)
	  (let*
	    ((rel-pos (sub
			(slot-value other-obj 'pos)
			(slot-value obj 'pos)))
	     (distance (magnitude rel-pos)))
	    (unless (= 0.0 distance)
	      (add-force obj
			 (mult ; gravitational force vector
			   (/ ; G * m / r^2
			     (* *G* (slot-value obj 'mass) (slot-value other-obj 'mass))
			     (expt distance 2))
			   (mult ; r hat
			     (/ 1 distance)
			     rel-pos)) :frame :global))))))

(defgeneric add-force (obj force &key)
  (:documentation "apply a force to obj, and update acc"))

(defmethod add-force ((obj space-object) (force vector-3) &key (frame :local))
  (with-slots (mass acc) obj
    (setf acc (add acc (mult (/ 1 mass)
			     (if (eq frame :global)
			       force
			       (rotate force (slot-value obj 'ang-pos))))))))

(defgeneric add-torque (obj torque &key)
  (:documentation "apply a torque to obj, and update acc"))

(defmethod add-torque ((obj space-object) (torque vector-3) &key (frame :local))
  (with-slots (inertia-tensor ang-acc) obj
    (setf ang-acc (add ang-acc (mult (inverse inertia-tensor)
				     (if (eq frame :global)
				       torque
				       (rotate torque (slot-value obj 'ang-pos))))))))

(defgeneric add-force-off-center (obj force pos &key)
  (:documentation "apply a force to obj, compute the associated torque, and update acc and ang-acc accordingly"))

(defmethod add-force-off-center ((obj space-object) (force vector-3) (pos vector-3) &key (frame :local))
  (add-force obj force :frame frame)
  (add-torque obj (cross pos force) :frame frame))

(defgeneric compute-forces (obj dt)
  (:documentation "compute the forces on obj, and update acc and ang-acc"))

(defmethod compute-forces :before ((obj space-object) dt)
  (setf (slot-value obj     'acc) (make-vector-3 0 0 0))
  (setf (slot-value obj 'ang-acc) (make-vector-3 0 0 0)))

(defmethod compute-forces ((obj space-object) dt)
  (compute-gravity obj))

(defgeneric integrate-acc-to-vel (obj dt)
  (:documentation "integrate acceleration to get velocity."))

(defmethod integrate-acc-to-vel ((obj space-object) dt)
  (with-slots (vel acc) obj
    (setf vel (add vel (mult dt acc)))))

(defgeneric integrate-ang-acc-to-ang-vel (obj dt)
  (:documentation "integrate angular acceleration to get angular velocity."))

(defmethod integrate-ang-acc-to-ang-vel ((obj space-object) dt)
  (with-slots (ang-vel ang-acc) obj
    (setf ang-vel (add ang-vel (mult dt ang-acc)))))

(defgeneric integrate-vel-to-pos (obj dt)
  (:documentation "integrate velocity to get position."))

(defmethod integrate-vel-to-pos ((obj space-object) dt)
  (with-slots (pos vel) obj
    (setf pos (add pos (mult dt vel)))))

(defgeneric integrate-ang-vel-to-ang-pos (obj dt)
  (:documentation "integrate angular velocity to get angular position."))

(defmethod integrate-ang-vel-to-ang-pos ((obj space-object) dt)
  (with-slots (ang-pos ang-vel) obj
    (with-slots (x y z) ang-vel
      (setf ang-pos (add ang-pos (mult dt (mult 0.5 (mult (make-quaternion 0 x y z) ang-pos)))))
      (normalize ang-pos))))

(defun timestep (dt)
  (setf *epoch-time* (+ dt *epoch-time*))
  (loop for obj in *all-objs* do
	(compute-forces obj dt))
  (loop for obj in *all-objs* do
	(integrate-acc-to-vel obj dt)
	(integrate-ang-acc-to-ang-vel obj dt)
	(integrate-vel-to-pos obj dt)
	(integrate-ang-vel-to-ang-pos obj dt)))

(defun print-timestep ()
  (format *state-output-stream* "begin-timestep ~a~%" *epoch-time*)
  (dolist (obj *all-objs*)
    (format *state-output-stream* "begin-object~%")
    (with-slots (x y z) (slot-value obj 'pos)
      (format *state-output-stream* "pos ~a ~a ~a~%" x y z))
    (with-slots (w x y z) (slot-value obj 'ang-pos)
      (let ((len (magnitude (make-vector-3 x y z))))
	(print (list w x y z len))
	(if (/= 0 len) ; print in angle-axis form (in degrees, because that's what OpenGL uses *shudder*)
	  (format *state-output-stream* "ang-pos ~a ~a ~a ~a~%"
		  (/ (* (* 2 (acos w)) 180) pi)
		  (/ x len)
		  (/ y len)
		  (/ z len))
	  (format *state-output-stream* "ang-pos ~a ~a ~a ~a~%"
		  0
		  1
		  0
		  0))))
    (format *state-output-stream* "radius 1~%")
    (format *state-output-stream* "end-object~%"))
  (format *state-output-stream* "end-timestep~%"))

(defun main-loop ()
  (without-floating-point-underflow
    (let ((current-time (get-internal-real-time)) prev-time)
      (loop
	(setf prev-time current-time)
	(setf current-time (get-internal-real-time))
	(check-for-high-level-keyboard-commands)
	(timestep (/ (- current-time prev-time) (/ internal-time-units-per-second *time-acceleration*)))
	(glut-post-redisplay)
	(glut-main-loop-event)))))
