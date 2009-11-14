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

(defclass relative-hud (hud-layer)
	((origin
		:initarg :origin
		:initform nil)
	(target
		:initarg :target
		:initform nil)))

(defmethod initialize-instance :after ((hud relative-hud) &rest stuff)
	(with-slots (name origin target buttons) hud
		(setf name (preval format nil "~a REL TO ~a" (slot-value target 'name) (slot-value origin 'name)))
		(setf (slot-value (second buttons) 'text) name)))

(let ((hud-marker-size 40))
	(defmethod draw-2d ((hud relative-hud) screen-size)
		(with-slots (origin target) hud
			(multiple-value-bind (pos visible) (project-position-to-hud (slot-value target 'pos) *current-modelview-matrix* (gethash target *projection-matrix-for-object*) screen-size)
				(gl-place-string
					(format nil "~aP ~a" (if visible "+" "-") (slot-value target 'name))
					(add pos (make-vector-2 (* -1 hud-marker-size) (+ 10 hud-marker-size))))
				(gl-place-string
					(format nil "~3$" (magnitude (sub (slot-value target 'pos) (slot-value origin 'pos))))
					(add pos (make-vector-2 (* -1 hud-marker-size) (+ -15 (* -1 hud-marker-size)))))
				(gl-begin-end *gl-line-loop*
					(gl-vertex-vector-2 (add pos (make-vector-2       hud-marker-size        hud-marker-size)))
					(gl-vertex-vector-2 (add pos (make-vector-2 (* -1 hud-marker-size)       hud-marker-size)))
					(gl-vertex-vector-2 (add pos (make-vector-2 (* -1 hud-marker-size) (* -1 hud-marker-size))))
					(gl-vertex-vector-2 (add pos (make-vector-2       hud-marker-size  (* -1 hud-marker-size)))))
				(if (not visible)
					(gl-begin-end *gl-lines*
						(gl-vertex-vector-2 (add pos (make-vector-2 (* -1 hud-marker-size) 0)))
						(gl-vertex-vector-2 (add pos (make-vector-2       hud-marker-size  0))))))
			(multiple-value-bind (pos visible) (project-position-to-hud (add (slot-value origin 'pos) (sub (slot-value target 'vel) (slot-value origin 'vel))) *current-modelview-matrix* (gethash target *projection-matrix-for-object*) screen-size)
				(gl-place-string
					(format nil "~aV ~a" (if visible "+" "-") (slot-value target 'name))
					(add pos (make-vector-2 (* -1 hud-marker-size) (+ 10 hud-marker-size))))
				(gl-place-string
					(format nil "~3$" (magnitude (sub (slot-value target 'vel) (slot-value origin 'vel))))
					(add pos (make-vector-2 (* -1 hud-marker-size) (+ -15 (* -1 hud-marker-size)))))
				(gl-begin-end *gl-lines*
					(gl-vertex-vector-2 (add pos (make-vector-2       0                      hud-marker-size)))
					(gl-vertex-vector-2 (add pos (make-vector-2       0                (* -1 hud-marker-size))))
					(gl-vertex-vector-2 (add pos (make-vector-2 (* -1 hud-marker-size)       0)))
					(gl-vertex-vector-2 (add pos (make-vector-2       hud-marker-size        0))))
				(if visible
					(gl-begin-end *gl-line-loop*
						(gl-vertex-vector-2 (add pos (make-vector-2 (* -0.5 hud-marker-size)         0)))
						(gl-vertex-vector-2 (add pos (make-vector-2         0                (*  0.5 hud-marker-size))))
						(gl-vertex-vector-2 (add pos (make-vector-2 (*  0.5 hud-marker-size)         0)))
						(gl-vertex-vector-2 (add pos (make-vector-2         0                (* -0.5 hud-marker-size))))))))))
