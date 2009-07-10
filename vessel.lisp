(defclass vessel (space-object)
	((max-torque
		:initarg :max-torque
		:initform (make-vector-3)
		:documentation "The maximum torque that the vessels attitude controls can exert along the three axes (a vector-3)")))

(defmethod compute-acc ((obj vessel) all-objs)
	(call-next-method))

(defmethod compute-ang-acc ((obj vessel) all-objs)
	(call-next-method)
	(format *error-output* "ang~a!~a~%" (list "6") *depressed-keys*)
	(cond
		((check-depressed-keys "8")
			(add-torque obj (make-vector-3 (* -1 (slot-value (slot-value obj 'max-torque) 'x)) 0 0)))
		((check-depressed-keys "2")
			(add-torque obj (make-vector-3       (slot-value (slot-value obj 'max-torque) 'x)  0 0)))
		((check-depressed-keys "4")
			(add-torque obj (make-vector-3 0 0       (slot-value (slot-value obj 'max-torque) 'z))))
		((check-depressed-keys "6")
			(add-torque obj (make-vector-3 0 0 (* -1 (slot-value (slot-value obj 'max-torque) 'z)))))
		((check-depressed-keys "1")
			(add-torque obj (make-vector-3 0       (slot-value (slot-value obj 'max-torque) 'y)  0)))
		((check-depressed-keys "3")
			(add-torque obj (make-vector-3 0 (* -1 (slot-value (slot-value obj 'max-torque) 'y)) 0)))))
