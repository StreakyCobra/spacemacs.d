(defun insert-datetime-stamp ()
  "Insert string of the current datetime stamp."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))
