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

(defvar *char-size* (make-vector-2 8 17))
(defvar *char-non-print* 94)

(defvar *char-set* (vector
	(vector #x00 #x00 #x00 #x00 #x18 #x18 #x00 #x18 #x18 #x18 #x3C #x3C #x3C #x18 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x24 #x66 #x66 #x66 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x6C #x6C #xFE #x6C #x6C #x6C #xFE #x6C #x6C #x00 #x00 #x00 #x00)
	(vector #x00 #x00 #x18 #x18 #x7C #xC6 #x86 #x06 #x06 #x7C #xC0 #xC2 #xC6 #x7C #x18 #x18 #x00)
	(vector #x00 #x00 #x00 #x00 #x86 #xC6 #x60 #x30 #x18 #x0C #xC6 #xC2 #x00 #x00 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x76 #xCC #xCC #xCC #xDC #x76 #x38 #x6C #x6C #x38 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x60 #x30 #x30 #x30 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x0C #x18 #x30 #x30 #x30 #x30 #x30 #x30 #x18 #x0C #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x30 #x18 #x0C #x0C #x0C #x0C #x0C #x0C #x18 #x30 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x00 #x00 #x66 #x3C #xFF #x3C #x66 #x00 #x00 #x00 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x00 #x00 #x18 #x18 #x7E #x18 #x18 #x00 #x00 #x00 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x30 #x18 #x18 #x18 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xFE #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x18 #x18 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x80 #xC0 #x60 #x30 #x18 #x0C #x06 #x02 #x00 #x00 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x38 #x6C #xC6 #xC6 #xD6 #xD6 #xC6 #xC6 #x6C #x38 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x7E #x18 #x18 #x18 #x18 #x18 #x18 #x78 #x38 #x18 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #xFE #xC6 #xC0 #x60 #x30 #x18 #x0C #x06 #xC6 #x7C #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x7C #xC6 #x06 #x06 #x06 #x3C #x06 #x06 #xC6 #x7C #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x1E #x0C #x0C #x0C #xFE #xCC #x6C #x3C #x1C #x0C #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x7C #xC6 #x06 #x06 #x06 #xFC #xC0 #xC0 #xC0 #xFE #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x7C #xC6 #xC6 #xC6 #xC6 #xFC #xC0 #xC0 #x60 #x38 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x30 #x30 #x30 #x30 #x18 #x0C #x06 #x06 #xC6 #xFE #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x7C #xC6 #xC6 #xC6 #xC6 #x7C #xC6 #xC6 #xC6 #x7C #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x78 #x0C #x06 #x06 #x06 #x7E #xC6 #xC6 #xC6 #x7C #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x00 #x18 #x18 #x00 #x00 #x00 #x18 #x18 #x00 #x00 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x30 #x18 #x18 #x00 #x00 #x00 #x18 #x18 #x00 #x00 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x06 #x0C #x18 #x30 #x60 #x30 #x18 #x0C #x06 #x00 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x7E #x00 #x00 #x7E #x00 #x00 #x00 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x60 #x30 #x18 #x0C #x06 #x0C #x18 #x30 #x60 #x00 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x18 #x18 #x00 #x18 #x18 #x18 #x0C #xC6 #xC6 #x7C #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x7C #xC0 #xDC #xDE #xDE #xDE #xC6 #xC6 #x7C #x00 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #xC6 #xC6 #xC6 #xC6 #xFE #xC6 #xC6 #x6C #x38 #x10 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #xFC #x66 #x66 #x66 #x66 #x7C #x66 #x66 #x66 #xFC #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x3C #x66 #xC2 #xC0 #xC0 #xC0 #xC0 #xC2 #x66 #x3C #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #xF8 #x6C #x66 #x66 #x66 #x66 #x66 #x66 #x6C #xF8 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #xFE #x66 #x62 #x60 #x68 #x78 #x68 #x62 #x66 #xFE #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #xF0 #x60 #x60 #x60 #x68 #x78 #x68 #x62 #x66 #xFE #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x3A #x66 #xC6 #xC6 #xDE #xC0 #xC0 #xC2 #x66 #x3C #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #xC6 #xC6 #xC6 #xC6 #xC6 #xFE #xC6 #xC6 #xC6 #xC6 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x3C #x18 #x18 #x18 #x18 #x18 #x18 #x18 #x18 #x3C #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x78 #xCC #xCC #xCC #x0C #x0C #x0C #x0C #x0C #x1E #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #xE6 #x66 #x66 #x6C #x78 #x78 #x6C #x66 #x66 #xE6 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #xFE #x66 #x62 #x60 #x60 #x60 #x60 #x60 #x60 #xF0 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #xC6 #xC6 #xC6 #xC6 #xC6 #xD6 #xFE #xFE #xEE #xC6 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #xC6 #xC6 #xC6 #xC6 #xCE #xDE #xFE #xF6 #xE6 #xC6 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x7C #xC6 #xC6 #xC6 #xC6 #xC6 #xC6 #xC6 #xC6 #x7C #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #xF0 #x60 #x60 #x60 #x60 #x7C #x66 #x66 #x66 #xFC #x00 #x00 #x00)
	(vector #x00 #x00 #x0E #x0C #x7C #xDE #xD6 #xC6 #xC6 #xC6 #xC6 #xC6 #xC6 #x7C #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #xE6 #x66 #x66 #x66 #x6C #x7C #x66 #x66 #x66 #xFC #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x7C #xC6 #xC6 #x06 #x0C #x38 #x60 #xC6 #xC6 #x7C #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x3C #x18 #x18 #x18 #x18 #x18 #x18 #x5A #x7E #x7E #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x7C #xC6 #xC6 #xC6 #xC6 #xC6 #xC6 #xC6 #xC6 #xC6 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x10 #x38 #x6C #xC6 #xC6 #xC6 #xC6 #xC6 #xC6 #xC6 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x6C #xEE #xFE #xD6 #xD6 #xD6 #xC6 #xC6 #xC6 #xC6 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #xC6 #xC6 #x6C #x7C #x38 #x38 #x7C #x6C #xC6 #xC6 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x3C #x18 #x18 #x18 #x18 #x3C #x66 #x66 #x66 #x66 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #xFE #xC6 #xC2 #x60 #x30 #x18 #x0C #x86 #xC6 #xFE #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x3C #x30 #x30 #x30 #x30 #x30 #x30 #x30 #x30 #x3C #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x02 #x06 #x0E #x1C #x38 #x70 #xE0 #xC0 #x80 #x00 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x3C #x0C #x0C #x0C #x0C #x0C #x0C #x0C #x0C #x3C #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xC6 #x6C #x38 #x10 #x00)
	(vector #x00 #x00 #xFF #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x0C #x18 #x30 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x76 #xCC #xCC #xCC #x7C #x0C #x78 #x00 #x00 #x00 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x7C #x66 #x66 #x66 #x66 #x6C #x78 #x60 #x60 #xE0 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x7C #xC6 #xC0 #xC0 #xC0 #xC6 #x7C #x00 #x00 #x00 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x76 #xCC #xCC #xCC #xCC #x6C #x3C #x0C #x0C #x1C #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x7C #xC6 #xC0 #xC0 #xFE #xC6 #x7C #x00 #x00 #x00 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x78 #x30 #x30 #x30 #x30 #x78 #x30 #x32 #x36 #x1C #x00 #x00 #x00)
	(vector #x00 #x78 #xCC #x0C #x7C #xCC #xCC #xCC #xCC #xCC #x76 #x00 #x00 #x00 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #xE6 #x66 #x66 #x66 #x66 #x76 #x6C #x60 #x60 #xE0 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x3C #x18 #x18 #x18 #x18 #x18 #x38 #x00 #x18 #x18 #x00 #x00 #x00)
	(vector #x00 #x3C #x66 #x66 #x06 #x06 #x06 #x06 #x06 #x06 #x0E #x00 #x06 #x06 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #xE6 #x66 #x6C #x78 #x78 #x6C #x66 #x60 #x60 #xE0 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x3C #x18 #x18 #x18 #x18 #x18 #x18 #x18 #x18 #x38 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #xC6 #xD6 #xD6 #xD6 #xD6 #xFE #xEC #x00 #x00 #x00 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x66 #x66 #x66 #x66 #x66 #x66 #xDC #x00 #x00 #x00 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x7C #xC6 #xC6 #xC6 #xC6 #xC6 #x7C #x00 #x00 #x00 #x00 #x00 #x00)
	(vector #x00 #xF0 #x60 #x60 #x7C #x66 #x66 #x66 #x66 #x66 #xDC #x00 #x00 #x00 #x00 #x00 #x00)
	(vector #x00 #x1E #x0C #x0C #x7C #xCC #xCC #xCC #xCC #xCC #x76 #x00 #x00 #x00 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #xF0 #x60 #x60 #x60 #x66 #x76 #xDC #x00 #x00 #x00 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x7C #xC6 #x0C #x38 #x60 #xC6 #x7C #x00 #x00 #x00 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x1C #x36 #x30 #x30 #x30 #x30 #xFC #x30 #x30 #x10 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x76 #xCC #xCC #xCC #xCC #xCC #xCC #x00 #x00 #x00 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x38 #x6C #xC6 #xC6 #xC6 #xC6 #xC6 #x00 #x00 #x00 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x6C #xFE #xD6 #xD6 #xD6 #xC6 #xC6 #x00 #x00 #x00 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #xC6 #x6C #x38 #x38 #x38 #x6C #xC6 #x00 #x00 #x00 #x00 #x00 #x00)
	(vector #x00 #xF8 #x0C #x06 #x7E #xC6 #xC6 #xC6 #xC6 #xC6 #xC6 #x00 #x00 #x00 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #xFE #xC6 #x60 #x30 #x18 #xCC #xFE #x00 #x00 #x00 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x0E #x18 #x18 #x18 #x18 #x70 #x18 #x18 #x18 #x0E #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x18 #x18 #x18 #x18 #x18 #x18 #x18 #x18 #x18 #x18 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x70 #x18 #x18 #x18 #x18 #x0E #x18 #x18 #x18 #x70 #x00 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xDC #x76 #x00 #x00)
	(vector #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00)))
