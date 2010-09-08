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

(defclass tea-torch (vessel)
  (()))

(defmethod initialize-instance :after ((tea-torch tea-torch) &key)
  (make-simple-thruster-setup tea-torch)
  (setf (slot-value (first (slot-value (getf (slot-value tea-torch 'thruster-groups) :main) 'thrusters)) 'max-thrust) (make-vector-3 0 0 -5000)))
