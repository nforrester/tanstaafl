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

(defun split-string (chr str)
  (loop
    for i = 0 then (1+ j)
    as j = (position chr str :start i)
    collect (subseq str i j)
    while j))

(defun symbol-to-string (sym)
  (format nil "~a" sym))

(defun list-strings-to-string (strings)
  (let ((target ""))
    (dolist (str strings)
      (setf target (concatenate 'string target str)))
    target))

(defun label-print (label value)
  (print (list label value))
  value)

(defun clamp (minimum maximum value)
  (cond
    ((<= minimum value maximum) value)
    ((< value minimum) minimum)
    ((< maximum value) maximum)))

(defmacro preval (&rest stuff) `(print ,stuff))

(defmacro lpreval (label &rest stuff)
  `(let ((x ,stuff))
     (print (list ,label x))
     x))
