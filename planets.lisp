(defclass spherical-body (space-object) ())

(defmethod draw ((obj spherical-body))
	(glut-solid-sphere (slot-value obj 'radius) 200 100))
