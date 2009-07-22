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
