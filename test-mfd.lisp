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

(defclass test-mfd (mfd)
  ((color
     :initarg :color
     :initform (make-color 1 1 1 1))))

(defmethod draw-2d ((mfd test-mfd) screen-size)
  (gl-matrix-mode *gl-projection*)
  (gl-load-identity)
  (glu-ortho2-d 0 1 0 1)
  (gl-matrix-mode *gl-modelview*)
  (gl-load-identity)
  (gl-clear *gl-depth-buffer-bit*)
  (gl-color (slot-value mfd 'color))
  (gl-begin-end *gl-quads*
		(gl-vertex2d 0 0)
		(gl-vertex2d 0 1)
		(gl-vertex2d 1 1)
		(gl-vertex2d 1 0))
  (gl-color (make-color 1 1 1 1))
  (gl-place-string (concatenate 'string `(,@(concatenate 'list "JELLO WORLD") #\Newline ,@(concatenate 'list "helo"))) (make-vector-2 .5 .5))
  (gl-place-string "jello world" (make-vector-2 .5 .5) :anchor-point (make-vector-2 1 -1)))
