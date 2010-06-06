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

(defclass orbital-elements ()
  ((major-body
     :initarg :major-body)
   (minor-body
     :initarg :minor-body)
   (x-axis
     :initarg :x-axis)
   (z-axis
     :initarg :z-axis)
   (semi-major-axis
     :initarg :semi-major-axis)
   (eccentricity
     :initarg :eccentricity)
   (inclination
     :initarg :inclination)
   (longitude-of-ascending-node
     :initarg :longitude-of-ascending-node)
   (argument-of-periapsis
     :initarg :argument-of-periapsis)
   (true-anomaly
     :initarg :true-anomaly)))

;;; Most of these formulas were found on page 121 of the manual for Orbiter 2006,
;;; where you can find a very nice description. I made up a minority of them myself.
;;;
;;; minor-body orbits major-body, and compute-elements will return the elements of this orbit.
;;;
;;; ref-plane-normal is the normal vector to the reference plane
;;; (the ecliptic plane is a common one to use in the solar system).
;;;
;;; ref-direction is any old vector you want (the vector from the sun to the earth at the
;;; vernal equinox is a common one to use in the solar system).
;;;
;;; ref-direction will be projected onto the plane defined by ref-plane-normal.
;;; Then you have a nice right-handed local coordinate system defined by the normalized projected
;;; vector for the x-axis, the normalized ref-plane-normal for the z-axis, and the z-axis cross the
;;; x-axis for the y-axis.
(defun compute-elements (major-body minor-body ref-plane-normal ref-direction)
  (let*
    ((z-axis (normalize ref-plane-normal))
     (x-axis (normalize (cross ref-plane-normal (cross ref-direction ref-plane-normal))))
     (y-axis (normalize (cross z-axis x-axis)))
     (r (sub (slot-value minor-body 'pos) (slot-value major-body 'pos)))
     (v (sub (slot-value minor-body 'vel) (slot-value major-body 'vel)))
     (h (let ((h (cross r v))) (if (= 0 (magnitude h)) (make-vector-3 0 0 1) h)))
     (n (cross z-axis h))
     (mu (* *G* (slot-value major-body 'mass)))
     (e-vector (mult (/ 1 mu) (sub (mult (- (expt (magnitude v) 2) (/ mu (magnitude r))) r) (mult (dot r v) v))))
     (e (magnitude e-vector))
     (i (acos (/ (dot h z-axis) (magnitude h)))))
    (make-instance 'orbital-elements
                   :major-body major-body
                   :minor-body minor-body
                   :x-axis x-axis
                   :z-axis z-axis
                   :semi-major-axis (/ (* -1 mu) (* 2 (- (/ (expt (magnitude v) 2) 2) (/ mu (magnitude r)))))
                   :eccentricity e
                   :inclination i
                   :longitude-of-ascending-node
                   (if (= i 0)
                     0 ; inclination is 0, so the expression below is undefined
                     (let ((big-omega (acos (/ (dot n x-axis) (magnitude n)))))
                       (if (< 0 (dot n y-axis))
                         big-omega
                         (- (* 2 pi) big-omega))))
                   :argument-of-periapsis
                   (let ((little-omega (cond
                                         ((= e 0) 0) ; eccentricity is 0, so both of the expressions below are undefined
                                         ((= i 0) (acos (/ (dot e-vector x-axis) (magnitude e-vector))))
                                         ; above, inclination is 0, so the expression below is undefined
                                         (t (acos (/ (dot n e-vector) (* (magnitude n) (magnitude e-vector))))))))
                     (if (< 0 (dot e-vector z-axis))
                       little-omega
                       (- (* 2 pi) little-omega)))
                   :true-anomaly
                   (let
                     ((upsilon (if (= e 0)
                                 (acos (/ (dot n r) (* (magnitude n) (magnitude r))))
                                 (acos (/ (dot e-vector r) (* (magnitude e-vector) (magnitude r))))))
                      (exceptional-case (if (= e 0)
                                          (< 0 (dot n v))
                                          (> 0 (dot r v)))))
                     (if (not exceptional-case)
                       upsilon
                       (- (* 2 pi) upsilon))))))

(defun orbital-period (elements)
  (let ((period (with-slots (semi-major-axis major-body) elements
                  (* 2 pi (sqrt (/ (expt semi-major-axis 3) (* *G* (slot-value major-body 'mass))))))))
    (if (realp period) period nil)))

(defun periapsis-radius (elements)
  (with-slots (semi-major-axis eccentricity) elements
    (* semi-major-axis (- 1 eccentricity))))

(defun apoapsis-radius (elements)
  (with-slots (semi-major-axis eccentricity) elements
    (if (> 1 eccentricity)
      (* semi-major-axis (+ 1 eccentricity))
      nil)))

(defun periapsis-time (elements)
  (with-slots (major-body minor-body eccentricity semi-major-axis true-anomaly) elements
    (let* ((eccentric-anomaly (acos (/ (- 1 (/ (magnitude (sub (slot-value major-body 'pos) (slot-value minor-body 'pos))) semi-major-axis)) eccentricity)))
           (mean-anomaly (- eccentric-anomaly (* eccentricity (sin eccentric-anomaly))))
           (period (* 2 pi (expt (/ (expt semi-major-axis 3) (* *G* (slot-value major-body 'mass))) 0.5)))
           (time-to-periapsis (* mean-anomaly (/ period (* 2 pi)))))
      (if (> 1 eccentricity)
	(if (< pi true-anomaly)
          time-to-periapsis
          (- period time-to-periapsis))
        (* -1 time-to-periapsis)))))
