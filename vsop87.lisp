;;;; Unfortunately, I don't know much about VSOP. ;.(
;;;; This algorithm is ripped straight out of the celestia-1.6.0 source code,
;;;; coefficents and all. I just made it a little prettier in translation.
;;;; I don't actually know how it works, beyond the concept of adding
;;;; huge polynomials to get an interesting function, which in this case,
;;;; approximates the orbits of the planets and the sun. Where the
;;;; coefficents come from, and why the polynomials are structured the
;;;; way they are, is unknown to me.

;;;; A VSOP series will be represented as a list of terms.
;;;; A VSOP term will be a list of 3 floats.
;;;; A VSOP series set will be a list of VSOP series.
;;;; In total we have a list of lists of lists.

(defun sum-series (series julian-millenium)
	(loop for term in series sum
		(* (first term) (cos (+ (second term) (* julian-millenium (third term)))))))

(defun evaluate-series-set (series-set julian-millenium)
	(let ((big-t (/ 1 julian-millenium)))
		(loop for series in series-set sum
			(progn
				(setf big-t (* big-t julian-millenium))
				(* big-t (sum-series series julian-millenium))))))

(defclass vsop-reference-point ()
	((longitude :initarg :longitude)
	(latitude   :initarg :latitude)
	(radius     :initarg :radius)
	(epoch      :initarg :epoch)))

;;; Takes the time in seconds since the epoch, and VSOP series sets for
;;; latitude, longitude, and radius (heliocentric spherical coordinates),
;;; and returns a vsop-reference-point
(defun vsop-compute-reference-point (epoch vsop-lat vsop-long vsop-rad)
	(let ((julian-millenium (- (/ epoch (* 60 60 24 365.25 1000)) (/ (* 30 365.25) 1000)))) ;Julian millenia since J2000
		(let
				((longitude (evaluate-series-set vsop-long julian-millenium))
				(latitude   (evaluate-series-set vsop-lat  julian-millenium))
				(radius     (evaluate-series-set vsop-rad  julian-millenium)))
			(setf radius    (* radius   *m-per-au*))
			(setf latitude  (- latitude (/ pi 2)))
			(setf longitude (+ longitude   pi))
			(make-instance 'vsop-reference-point
				:longitude longitude
				:latitude latitude
				:radius radius
				:epoch epoch))))

;;; Takes the time in seconds since the epoch, VSOP series sets for
;;; latitude, longitude, and radius (heliocentric spherical coordinates),
;;; an interval in seconds, and 2 vsop-reference-points. If the time is
;;; between the first and second reference points, it interpolates a
;;; position between them. Otherwise, it computes new reference points an
;;; interval apart. The function returns a vector-3, with the HELIOCENTRIC
;;; position, and the two reference points that were actually used.
;;; Careful with that heliocentric position. Wouldn't want to forget to add
;;; it to the position of Sol.
(defun vsop-compute-position (epoch vsop-lat vsop-long vsop-rad interval vsop-ref-1 vsop-ref-2)
	(if (not (<= (slot-value vsop-ref-1 'epoch) epoch (slot-value vsop-ref-2 'epoch)))
		(progn
			(setf vsop-ref-1 (vsop-compute-reference-point             epoch  vsop-lat vsop-long vsop-rad))
			(setf vsop-ref-2 (vsop-compute-reference-point (+ interval epoch) vsop-lat vsop-long vsop-rad))))
	(let*
			((epoch-1 (slot-value vsop-ref-1 'epoch))
			(epoch-2 (slot-value vsop-ref-2 'epoch))
			(interval (- epoch-2 epoch-1))
			(short-interval (- epoch epoch-1))
			(longitude (+ (slot-value vsop-ref-1 'longitude) (* short-interval (/ (- (slot-value vsop-ref-2 'longitude) (slot-value vsop-ref-1 'longitude)) interval))))
			(latitude (+ (slot-value vsop-ref-1 'latitude) (* short-interval (/ (- (slot-value vsop-ref-2 'latitude) (slot-value vsop-ref-1 'latitude)) interval))))
			(radius (+ (slot-value vsop-ref-1 'radius) (* short-interval (/ (- (slot-value vsop-ref-2 'radius) (slot-value vsop-ref-1 'radius)) interval)))))
		(values
			(make-vector-3
				(* (cos longitude) (sin latitude) radius)
				(* (cos latitude) radius)
				(* -1 (sin longitude) (sin latitude) radius))
			vsop-ref-1
			vsop-ref-2)))
