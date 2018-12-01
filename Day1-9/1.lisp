
(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defun advent-one (filename)
  (reduce '+ (map 'list (lambda (x) (parse-integer x)) (get-file filename))))
