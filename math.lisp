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

(setf pi (coerce pi 'double-float)) ; Because pi doesn't need to be a long-float
(defvar *degrees-per-radian* (/ 360 (* 2 pi))
  (:documentation "All civilized people use radians. Unfortunately, most people are savages."))

; I would do vectors and matrices in a more general way, accounting for
; different possible sizes, but I only need a limited selection of sizes,
; so this is simpler. This is math for an engineer, not math for a
; mathematician.

(defclass vector-3 ()
  ((x :initarg :x :initform 0)
   (y :initarg :y :initform 0)
   (z :initarg :z :initform 0)))

(defun make-vector-3 (&optional (x 0.0) (y 0.0) (z 0.0))
  (make-instance 'vector-3 :x x :y y :z z))

(defgeneric print-math (out-stream vec)
  (:documentation "print."))

(defmethod print-math (out-stream (vec vector-3))
  (with-slots (x y z) vec
    (format out-stream "(~a ~a ~a)~%" x y z))
  vec)

(defgeneric add (one two)
  (:documentation "addition"))

(defmethod add ((one vector-3) (two vector-3))
  (with-slots ((ox x) (oy y) (oz z)) one
    (with-slots ((tx x) (ty y) (tz z)) two
      (make-vector-3
        (+ ox tx)
        (+ oy ty)
        (+ oz tz)))))

(defgeneric mult (scalar something)
  (:documentation "scalar multiplication. If scalar is actually the same thing as something, then element-wise multiplication is performed, like .* in MATLAB."))

(defmethod mult (scalar (vec vector-3))
  (with-slots (x y z) vec
    (make-vector-3
      (* scalar x)
      (* scalar y)
      (* scalar z))))

(defmethod mult ((v1 vector-3) (v2 vector-3))
  (with-slots ((x1 x) (y1 y) (z1 z)) v1
    (with-slots ((x2 x) (y2 y) (z2 z)) v2
      (make-vector-3
        (* x1 x2)
        (* y1 y2)
        (* z1 z2)))))

(defgeneric dot (one two)
  (:documentation "dot product"))

(defmethod dot ((one vector-3) (two vector-3))
  (with-slots ((ox x) (oy y) (oz z)) one
    (with-slots ((tx x) (ty y) (tz z)) two
      (+
        (* ox tx)
        (* oy ty)
        (* oz tz)))))

(defgeneric cross (one two)
  (:documentation "cross product"))

(defmethod cross ((one vector-3) (two vector-3))
  (with-slots ((ox x) (oy y) (oz z)) one
    (with-slots ((tx x) (ty y) (tz z)) two
      (make-vector-3
        (- (* oy tz) (* oz ty))
        (- (* oz tx) (* ox tz))
        (- (* ox ty) (* oy tx))))))

(defgeneric sub (one two)
  (:documentation "subtraction"))

(defmethod sub ((one vector-3) (two vector-3))
  (add one (mult -1 two)))

(defgeneric magnitude (vec)
  (:documentation "magnitude of a vector"))

(defmethod magnitude ((vec vector-3))
  (with-slots (x y z) vec
    (expt (+
            (expt x 2)
            (expt y 2)
            (expt z 2)) 0.5)))

(defclass vector-2 ()
  ((x :initarg :x :initform 0)
   (y :initarg :y :initform 0)))

(defun make-vector-2 (&optional (x 0.0) (y 0.0))
  (make-instance 'vector-2 :x x :y y))

(defmethod print-math (out-stream (vec vector-2))
  (with-slots (x y) vec
    (format out-stream "(~a ~a)~%" x y))
  vec)

(defmethod add ((one vector-2) (two vector-2))
  (with-slots ((ox x) (oy y)) one
    (with-slots ((tx x) (ty y)) two
      (make-vector-2
        (+ ox tx)
        (+ oy ty)))))

(defmethod mult (scalar (vec vector-2))
  (with-slots (x y) vec
    (make-vector-2
      (* scalar x)
      (* scalar y))))

(defmethod mult ((v1 vector-2) (v2 vector-2))
  (with-slots ((x1 x) (y1 y)) v1
    (with-slots ((x2 x) (y2 y)) v2
      (make-vector-2
        (* x1 x2)
        (* y1 y2)))))

(defmethod dot ((one vector-2) (two vector-2))
  (with-slots ((ox x) (oy y)) one
    (with-slots ((tx x) (ty y)) two
      (+
        (* ox tx)
        (* oy ty)))))

(defmethod cross ((one vector-2) (two vector-2))
  (with-slots ((ox x) (oy y)) one
    (with-slots ((tx x) (ty y)) two
      (- (* ox ty) (* oy tx)))))

(defmethod sub ((one vector-2) (two vector-2))
  (add one (mult -1 two)))

(defmethod magnitude ((vec vector-2))
  (with-slots (x y) vec
    (expt (+
            (expt x 2)
            (expt y 2)) 0.5)))

(defgeneric distance (v1 v2)
  (:documentation "distance between 2 things (like the tips of two vectors)"))

(defmethod distance ((v1 vector-3) (v2 vector-3))
  (magnitude (sub v1 v2)))

(defgeneric normalize (unnormalized)
  (:documentation "Normalize something, like a vector, or a quaternion"))

(defmethod normalize ((vec vector-3))
  (let ((len (magnitude vec)))
    (with-slots (x y z) vec
      (if (/= len 0)
        (progn
          (setf x (/ x len))
          (setf y (/ y len))
          (setf z (/ z len)))
        (progn
          (setf x 0)
          (setf y 0)
          (setf z 0)))))
  vec)

(defclass quaternion ()
  ((w :initarg :w :initform 1)
   (x :initarg :x :initform 0)
   (y :initarg :y :initform 0)
   (z :initarg :z :initform 0)))

(defun make-quaternion (&optional (w 1.0) (x 0.0) (y 0.0) (z 0.0))
  (make-instance 'quaternion :w w :x x :y y :z z))

(defmethod print-math (out-stream (quat quaternion))
  (with-slots (w x y z) quat
    (format out-stream "(~a ~a ~a ~a)~%" w x y z))
  quat)

(defmethod add ((one quaternion) (two quaternion))
  (with-slots ((ow w) (ox x) (oy y) (oz z)) one
    (with-slots ((tw w) (tx x) (ty y) (tz z)) two
      (make-quaternion
        (+ ow tw)
        (+ ox tx)
        (+ oy ty)
        (+ oz tz)))))

(defmethod mult ((one quaternion) (two quaternion))
  (with-slots ((ow w) (ox x) (oy y) (oz z)) one
    (with-slots ((tw w) (tx x) (ty y) (tz z)) two
      (make-quaternion
        (+ (* ow tw) (* -1 ox tx) (* -1 oy ty) (* -1 oz tz))
        (+ (* ow tx) (*    ox tw) (*    oy tz) (* -1 oz ty))
        (+ (* ow ty) (* -1 ox tz) (*    oy tw) (*    oz tx))
        (+ (* ow tz) (*    ox ty) (* -1 oy tx) (*    oz tw))))))

(defmethod mult (scalar (quat quaternion))
  (with-slots (w x y z) quat
    (make-quaternion
      (* scalar w)
      (* scalar x)
      (* scalar y)
      (* scalar z))))

(defmethod magnitude ((quat quaternion))
  (with-slots (w x y z) quat
    (expt
      (+
        (expt w 2)
        (expt x 2)
        (expt y 2)
        (expt z 2))
      0.5)))

(defmethod normalize ((quat quaternion))
  (let ((len (magnitude quat)))
    (with-slots (w x y z) quat
      (if (/= len 0)
        (progn
          (setf w (/ w len))
          (setf x (/ x len))
          (setf y (/ y len))
          (setf z (/ z len)))
        (progn
          (setf w 1)
          (setf x 0)
          (setf y 0)
          (setf z 0)))))
  quat)

(defgeneric inverse (thing)
  (:documentation "compute the inverse of something"))

(defmethod inverse ((quat quaternion))
  (let ((mag-squared (expt (magnitude quat) 2)))
    (with-slots (w x y z) quat
      (make-quaternion
        (/       w  mag-squared)
        (/ (* -1 x) mag-squared)
        (/ (* -1 y) mag-squared)
        (/ (* -1 z) mag-squared)))))

(defgeneric rotate (thing-to-rotate thing-to-rotate-by)
  (:documentation "rotates one thing by another"))

(defmethod rotate ((vec vector-3) (quat quaternion))
  (with-slots (x y z) vec
    (with-slots (x y z) (mult quat (mult (make-quaternion 0 x y z) (inverse quat)))
      (make-vector-3 x y z))))

(defclass matrix-3-3 ()
  ((m11 :initarg :m11 :initform 1)
   (m12 :initarg :m12 :initform 0)
   (m13 :initarg :m13 :initform 0)
   (m21 :initarg :m21 :initform 0)
   (m22 :initarg :m22 :initform 1)
   (m23 :initarg :m23 :initform 0)
   (m31 :initarg :m31 :initform 0)
   (m32 :initarg :m32 :initform 0)
   (m33 :initarg :m33 :initform 1)))

(defun make-matrix-3-3 (&optional
                         (m11 1) (m12 0) (m13 0)
                         (m21 0) (m22 1) (m23 0)
                         (m31 0) (m32 0) (m33 1))
  (make-instance 'matrix-3-3
                 :m11 m11 :m12 m12 :m13 m13
                 :m21 m21 :m22 m22 :m23 m23
                 :m31 m31 :m32 m32 :m33 m33))

(defmethod print-math (out-stream (mat matrix-3-3))
  (with-slots
    (m11 m12 m13
     m21 m22 m23
     m31 m32 m33) mat
    (format out-stream "((~a ~a ~a)~% (~a ~a ~a)~% (~a ~a ~a))~%"
            m11 m12 m13
            m21 m22 m23
            m31 m32 m33))
  mat)

(defmethod mult (scalar (mat matrix-3-3))
  (with-slots (m11 m12 m13
               m21 m22 m23
               m31 m32 m33) mat
    (make-matrix-3-3
      (* scalar m11)
      (* scalar m12)
      (* scalar m13)
      (* scalar m21)
      (* scalar m22)
      (* scalar m23)
      (* scalar m31)
      (* scalar m32)
      (* scalar m33))))

(defmethod mult ((mat matrix-3-3) (vec vector-3))
  (with-slots (x y z) vec
    (with-slots (m11 m12 m13
                 m21 m22 m23
                 m31 m32 m33) mat
      (make-vector-3
        (+
          (* x m11)
          (* y m12)
          (* z m13))
        (+
          (* x m21)
          (* y m22)
          (* z m23))
        (+
          (* x m31)
          (* y m32)
          (* z m33))))))

(defgeneric determinant (mat)
  (:documentation "compute the determinant of a matrix"))

(defmethod determinant ((mat matrix-3-3))
  (with-slots (m11 m12 m13
               m21 m22 m23
               m31 m32 m33) mat
    (-
      (+
        (* m11 m22 m33)
        (* m12 m23 m31)
        (* m13 m21 m32))
      (+
        (* m13 m22 m31)
        (* m12 m21 m33)
        (* m11 m23 m32)))))

(defmacro det-2-2 (a b c d)
  `(- (* ,a ,d) (* ,c ,b)))

(defgeneric transpose (mat)
  (:documentation "transpose a matrix"))

(defmethod transpose ((mat matrix-3-3))
  (with-slots (m11 m12 m13
               m21 m22 m23
               m31 m32 m33) mat
    (make-matrix-3-3
      m11 m21 m31
      m12 m22 m32
      m13 m23 m33)))

(defmethod inverse ((mat matrix-3-3))
  (with-slots (m11 m12 m13
               m21 m22 m23
               m31 m32 m33) mat
    (mult
      (/ 1 (determinant mat))
      (transpose (make-matrix-3-3
                   (det-2-2 m22 m23 m32 m33)
                   (* -1 (det-2-2 m21 m23 m31 m33))
                   (det-2-2 m21 m22 m31 m32)
                   (* -1 (det-2-2 m12 m13 m32 m33))
                   (det-2-2 m11 m13 m31 m33)
                   (* -1 (det-2-2 m11 m12 m31 m32))
                   (det-2-2 m12 m13 m22 m23)
                   (* -1 (det-2-2 m11 m13 m21 m23))
                   (det-2-2 m11 m12 m21 m22))))))

(defclass matrix-2-2 ()
  ((m11 :initarg :m11 :initform 1)
   (m12 :initarg :m12 :initform 0)
   (m21 :initarg :m21 :initform 0)
   (m22 :initarg :m22 :initform 1)))

(defun make-matrix-2-2 (&optional
                         (m11 1) (m12 0)
                         (m21 0) (m22 1))
  (make-instance 'matrix-2-2
                 :m11 m11 :m12 m12
                 :m21 m21 :m22 m22))

(defvar *reflect-x-matrix-2* (make-matrix-2-2 -1 0 0 1))
(defvar *reflect-y-matrix-2* (make-matrix-2-2 1 0 0 -1))

(defmethod print-math (out-stream (mat matrix-2-2))
  (with-slots
    (m11 m12
     m21 m22) mat
    (format out-stream "((~a ~a)~% (~a ~a))~%"
            m11 m12
            m21 m22))
  mat)

(defmethod mult (scalar (mat matrix-2-2))
  (with-slots (m11 m12
               m21 m22) mat
    (make-matrix-2-2
      (* scalar m11)
      (* scalar m12)
      (* scalar m21)
      (* scalar m22))))

(defmethod mult ((mat matrix-2-2) (vec vector-2))
  (with-slots (x y) vec
    (with-slots (m11 m12
                 m21 m22) mat
      (make-vector-2
        (+
          (* x m11)
          (* y m12))
        (+
          (* x m21)
          (* y m22))))))

(defmethod determinant ((mat matrix-2-2))
  (with-slots (m11 m12
               m21 m22) mat
    (- (* m11 m22)
       (* m12 m21))))

(defmethod transpose ((mat matrix-2-2))
  (with-slots (m11 m12
               m21 m22) mat
    (make-matrix-2-2
      m11 m21
      m12 m22)))

(defmethod inverse ((mat matrix-2-2))
  (with-slots (m11 m12
               m21 m22) mat
    (mult
      (/ 1 (determinant mat))
      (make-matrix-2-2
        m11 (* -1 m12)
        (* -1 m21) m22))))

(defclass vector-3-spherical ()
  ((longitude :initarg :longitude :initform 0)
   (latitude :initarg :latitude :initform 0)
   (radius :initarg :radius :initform 0)))

(defun make-vector-3-spherical (&optional (longitude 0.0) (latitude 0.0) (radius 0.0))
  (make-instance 'vector-3-spherical :longitude longitude :latitude latitude :radius radius))

(defmethod print-math (out-stream (vec vector-3-spherical))
  (with-slots (longitude latitude radius) vec
    (format out-stream "(~a ~a ~a)~%" longitude latitude radius))
  vec)

(defgeneric convert (target-type argument)
  (:documentation "Return an object of class that is equivalent to argument, or nil if the conversion is impossible (or not implemented ;-) )."))

(defmethod convert (target-type (argument vector-3-spherical))
  (with-slots (longitude latitude radius) argument
    (cond
      ((eq target-type 'vector-3)
       (make-vector-3
         (* radius (cos longitude) (cos latitude))
         (* radius (cos longitude) (sin latitude))
         (* radius (sin longitude))))
      (t nil))))

(defun sign(x)
  (cond
    ((> x 0)  1)
    ((= x 0)  0)
    ((< x 0) -1)))

(defun atan2 (x y)
  (let ((angle (if (not (= x 0)) (atan (abs (/ y x))))))
    (cond
      ((> x 0) (* (sign y) angle))
      ((< x 0) (* (sign y) (- pi angle)))
      ((= x 0) (* (sign y) (/ pi 2))))))

(defmethod convert (target-type (argument vector-3))
  (with-slots (x y z) argument
    (cond
      ((eq target-type 'vector-3-spherical)
       (make-vector-3-spherical
         (if (= x y z 0) 0 (* -1 (- (acos (/ z (expt (+ (expt x 2) (expt y 2) (expt z 2)) 0.5))) (/ pi 2))))
         (atan2 x y)
         (expt (+ (expt x 2) (expt y 2) (expt z 2)) 0.5)))
      (t nil))))
