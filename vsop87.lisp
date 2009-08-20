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

;;;; A VSOP series will be an object of the class vsop-series
;;;; A VSOP term will be a list of 3 floats.
;;;; A VSOP series set will be a list of VSOP series.

(defclass vsop-series ()
	((alpha
		:initform 0
		:initarg :alpha)
	(terms
		:initform ()
		:initarg :terms)))

;;;; All the VSOP87 data is contained in the subdirectory vsop87

(defun read-int-from-string (str start end)
	(read-from-string (subseq str start end)))

(defun read-fixed-point-as-double-from-string (str start end)
	(read-from-string (concatenate 'string (subseq str start end) "d0")))

;;; This function reads the VSOP87 data files.
;;; This function does not do robust text processing.
;;; It relies heavily on the fact that the VSOP87 data files
;;; have a well defined structure, laid out in vsop87.doc.
(defun vsop87-file-reader (filename)
	(with-open-file (file filename)	
		(let ((variable-to-series-set-plist ()))
			(loop for line = (read-line file nil) while line do
				(let
						((variable (read-from-string (concatenate 'string ":"
							(let ((coord-index (read-int-from-string line 41 42)))
								(subseq (subseq line (position #\( line) (position #\) line)) coord-index (1+ coord-index)))))))
					(setf (getf variable-to-series-set-plist variable)
						(append (getf variable-to-series-set-plist variable)
							(list (make-instance 'vsop-series
								:alpha (read-int-from-string line 59 60)
								:terms
									(let (term-line)
										(loop for i from 1 upto (read-int-from-string line 60 67) collecting
											(progn
												(setf term-line (read-line file))
												(list
													(read-fixed-point-as-double-from-string term-line  79  97)
													(read-fixed-point-as-double-from-string term-line  97 111)
													(read-fixed-point-as-double-from-string term-line 111 131)))))))))))
			variable-to-series-set-plist)))

(defmacro build-vsop87-for-body (body)
	`(let ((vsop87-plist (vsop87-file-reader ,(concatenate 'string "vsop87/VSOP87E." (subseq body 0 3)))))
		(defvar ,(read-from-string (format nil "*vsop-series-set-~a-x*" body)) (getf vsop87-plist :x))
		(defvar ,(read-from-string (format nil "*vsop-series-set-~a-y*" body)) (getf vsop87-plist :y))
		(defvar ,(read-from-string (format nil "*vsop-series-set-~a-z*" body)) (getf vsop87-plist :z))))

(build-vsop87-for-body "sun")
(build-vsop87-for-body "mercury")
(build-vsop87-for-body "venus")
(build-vsop87-for-body "earth")
(build-vsop87-for-body "mars")
(build-vsop87-for-body "jupiter")
(build-vsop87-for-body "saturn")
(build-vsop87-for-body "uranus")
(build-vsop87-for-body "neptune")

(defun evaluate-series (series julian-millenium)
	(*
		(expt julian-millenium (slot-value series 'alpha))
		(loop for term in (slot-value series 'terms) sum
			(*
				(first term)
				(cos (+
					(second term)
					(* julian-millenium (third term))))))))

(defun evaluate-series-set (series-set julian-millenium)
	(loop for series in series-set sum
		(evaluate-series series julian-millenium)))

(defclass vsop-reference-point ()
	((pos
		:initarg :pos
		:initform (make-vector-3-spherical 0 0 0))
	(epoch
		:initarg :epoch)))

;;; Takes the time in seconds since the epoch, and VSOP series sets for
;;; latitude, longitude, and radius (heliocentric spherical coordinates),
;;; and returns a vsop-reference-point
(defun vsop-compute-reference-point (epoch vsop-x vsop-y vsop-z)
	(let ((julian-millenium (- (/ epoch (* 60 60 24 365.25 1000)) (/ (* 30 365.25) 1000)))) ;Julian millenia since J2000
		(make-instance 'vsop-reference-point
			:epoch epoch
			:pos (convert 'vector-3-spherical (make-vector-3
				(* *m-per-au* (evaluate-series-set vsop-x julian-millenium))
				(* *m-per-au* (evaluate-series-set vsop-y julian-millenium))
				(* *m-per-au* (evaluate-series-set vsop-z julian-millenium)))))))

;;; Takes the time in seconds since the epoch, VSOP series sets for
;;; latitude, longitude, and radius (heliocentric spherical coordinates),
;;; an interval in seconds, and 2 vsop-reference-points. If the time is
;;; between the first and second reference points, it interpolates a
;;; position between them. Otherwise, it computes new reference points an
;;; interval apart. The function returns a vector-3, with the HELIOCENTRIC
;;; position, and the two reference points that were actually used.
;;; Careful with that heliocentric position. Wouldn't want to forget to add
;;; it to the position of Sol.
(defun vsop-compute-position (epoch vsop-x vsop-y vsop-z interval vsop-ref-1 vsop-ref-2)
	(if (not (<= (slot-value vsop-ref-1 'epoch) epoch (slot-value vsop-ref-2 'epoch)))
		(progn
			(setf vsop-ref-1 (vsop-compute-reference-point             epoch  vsop-x vsop-y vsop-z))
			(setf vsop-ref-2 (vsop-compute-reference-point (+ interval epoch) vsop-x vsop-y vsop-z))))
	(let*
			((epoch-1 (slot-value vsop-ref-1 'epoch))
			(epoch-2 (slot-value vsop-ref-2 'epoch))
			(interval (- epoch-2 epoch-1))
			(short-interval (- epoch epoch-1)))
		(values
			(convert 'vector-3 (with-slots ((vsop-ref-1-pos pos)) vsop-ref-1 (with-slots ((vsop-ref-2-pos pos)) vsop-ref-2
				(make-vector-3-spherical
					(+ (slot-value vsop-ref-1-pos 'longitude) (* short-interval (/ (- (slot-value vsop-ref-2-pos 'longitude) (slot-value vsop-ref-1-pos 'longitude)) interval)))
					(+ (slot-value vsop-ref-1-pos 'latitude) (* short-interval (/ (- (slot-value vsop-ref-2-pos 'latitude) (slot-value vsop-ref-1-pos 'latitude)) interval)))
					(+ (slot-value vsop-ref-1-pos 'radius) (* short-interval (/ (- (slot-value vsop-ref-2-pos 'radius) (slot-value vsop-ref-1-pos 'radius)) interval)))))))
			vsop-ref-1
			vsop-ref-2)))
