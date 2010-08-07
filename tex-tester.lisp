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

(defclass tex-tester (vessel)
  ((texture
     :initarg :texture
     :initform (load-texture "f-pentomino" 4 4))))

(let ((default-material (make-instance 'material)))
  (defmethod draw ((obj tex-tester))
    (set-material *gl-front-and-back* default-material)
    (gl-bind-texture *gl-texture-2d* (slot-value obj 'texture))
    (gl-rotated 90d0 0d0 1d0 0d0)
    (gl-begin-end *gl-quads*
      (gl-tex-coord2d 0.0 0.0)
      (gl-vertex2d -10.0 -10.0)
      (gl-tex-coord2d 4.0 0.0)
      (gl-vertex2d 10.0 -10.0)
      (gl-tex-coord2d 4.0 4.0)
      (gl-vertex2d 10.0 10.0)
      (gl-tex-coord2d 0.0 4.0)
      (gl-vertex2d -10.0 10.0))))
