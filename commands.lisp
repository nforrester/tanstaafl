(let ((char-buffer ""))
	(defun get-command () ; Non-blocking, returns nil or a whole line from *command-input-stream*
		(let (found-newline)
			(loop
				(let ((single-char (read-char-no-hang *command-input-stream*)))
					(cond
						((eq nil single-char)
							(setf found-newline nil)
							(return))
						((eq #\Newline single-char)
							(setf found-newline t)
							(return))
						(t
							(setf char-buffer (concatenate 'string char-buffer (list single-char)))))))
			(if (eq t found-newline)
				(let ((return-value char-buffer))
					(setf char-buffer "")
					return-value)))))

(defun split-string (str)
	(loop
		for i = 0 then (1+ j)
		as j = (position #\Space str :start i)
		collect (subseq str i j)
		while j))

(defvar *depressed-keys* ())

(defun check-depressed-keys (str)
	(< 0 (loop for key in *depressed-keys* counting (string= str key))))

(defun process-commands ()
	(loop
		(let ((command (get-command)))
			(if (eq nil command)
				(return)
				(let ((command-list (split-string command)))
					(format *error-output* "command-list: ~a~%" command-list)
					(cond
						((string= "key" (first command-list))
							(if (string= "down" (third command-list))
								(if (not (check-depressed-keys (second command-list)))
									(push (second command-list) *depressed-keys*)))
							(if (string= "up" (third command-list))
								(setf *depressed-keys* (mapcan #'(lambda (x) (if (string= x (second command-list)) nil (list x))) *depressed-keys*))))))))))
