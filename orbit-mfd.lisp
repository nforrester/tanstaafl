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

(defclass orbit-mfd (mfd)
	((major-body
		:initarg :major-body
		:documentation "The body to plot orbits around")
	(minor-bodies
		:initarg :minor-bodies
		:documentation "A list of the bodies whose orbits will be plotted")
	(ref-plane-normal
		:initarg :ref-plane-normal
		:initform (make-vector-3 0 0 1)
		:documentation "A vector normal to the plane from which inclination is measured")
	(ref-direction
		:initarg :ref-direction
		:initform (make-vector-3 1 0 0))))

(defmethod draw-2d ((mfd orbit-mfd) screen-size)
	(with-slots (major-body minor-bodies ref-plane-normal ref-direction) mfd
		(let*
				((all-elements (loop for minor-body in minor-bodies collecting
					(compute-elements major-body minor-body ref-plane-normal ref-direction)))
				(half-scene-width (loop for elements in all-elements maximizing
					(with-slots (inclination eccentricity) elements
						(if (< eccentricity 1)
							(apoapsis-radius elements)
							(* 1.5 (periapsis-radius elements)))))))
			(gl-matrix-mode *gl-projection*)
			(gl-load-identity)
			(gl-ortho
				(* -1 half-scene-width) half-scene-width
				(* -1 half-scene-width) half-scene-width
				half-scene-width (* -1 half-scene-width))
			(gl-matrix-mode *gl-modelview*)
			(gl-load-identity)
			(gl-clear *gl-depth-buffer-bit*)
			(gl-color (make-color 0 1 0 .8))
			(loop
					for minor-body in minor-bodies
					for elements in all-elements do
				(with-slots
						(semi-major-axis
						eccentricity
						inclination
						longitude-of-ascending-node
						argument-of-periapsis
						true-anomaly) elements

					(gl-push-matrix) ;; Draw a radial line indicating the position of the minor body.

					;; align the world coordinate system with the orbital elements coordinate system (which the camera is using).
					(let ((crossed (cross (make-vector-3 0 0 1) ref-plane-normal)))
						(gl-rotate-angle-axis (asin (/ (magnitude crossed) (* 1 (magnitude ref-plane-normal)))) crossed))
					(let*
							((new-x-axis (normalize (cross ref-plane-normal (cross ref-direction ref-plane-normal))))
							(crossed (cross (make-vector-3 1 0 0) new-x-axis)))
						(gl-rotate-angle-axis (asin (/ (magnitude crossed) (* 1 (magnitude new-x-axis)))) crossed))

					(gl-begin-end *gl-lines*
						(gl-vertex3d 0 0 0)
						(gl-vertex-vector-3 (sub (slot-value minor-body 'pos) (slot-value major-body 'pos))))
					(gl-pop-matrix)

					(gl-push-matrix) ;; Draw the ellipse
					(gl-rotate-angle-axis longitude-of-ascending-node (make-vector-3 0 0 1))
					(gl-rotate-angle-axis inclination (make-vector-3 1 0 0))
					(gl-rotate-angle-axis argument-of-periapsis (make-vector-3 0 0 1))
					(gl-begin-end *gl-line-loop*
						(loop
								for theta from 0 to (* 2 pi) by (/ (* 2 pi) 150)
								for r = (/ (* semi-major-axis (- 1 (expt eccentricity 2))) (- 1 (* eccentricity (cos theta))))
								do
							(gl-vertex3d (* r (sin theta)) (* r (cos theta)) 0)))
					(gl-pop-matrix))))))
