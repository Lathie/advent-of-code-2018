;; from stack overflow
(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

;; Straightforward map (parse-integer) reduse (+)
(defun advent-one-one (filename)
  (reduce '+ (map 'list (lambda (x) (parse-integer x)) (get-file filename))))

(defun run-one-iter (init-value listicle)
  (let ((ints (push init-value listicle)))
    (map 'list (lambda (x) (reduce '+ x)) (loop for i from 0 to (length ints)
                                             collect (subseq ints 0 i)))))

(defun add-to-map (ht value)
  (if (gethash value ht)
      (format t "Finally the answer is ~D." value)
      (setf (gethash value ht) nil)))

;; This shit times out ): Just did some python instead :<1
(defun advent-one-two (filename)
  (let* ((file (get-file filename))
         (ints (map 'list (lambda (x) (parse-integer x)) file))
         (ht (make-hash-table))
         (cur 0))
    (loop for thing = (run-one-iter cur ints) do
         (if (reduce (lambda (x y) (or x y)) (map 'list (lambda (x) (add-to-map ht x)) thing))
             (return "DONE")
             (setq cur (elt (last (run-one-iter cur ints)) 0))))))

;; map (parse-integer) ;; map (each subseq)
(defun advent-one-two-fuck (filename)
  (let* ((file (get-file filename))
         (ints-raw (map 'list (lambda (x) (parse-integer x)) file))
         (ints (append ints-raw ints-raw ints-raw ints-raw ints-raw ints-raw ints-raw)))
    (map 'list (lambda (x) (reduce '+ x)) (loop for i from 0 to (length ints)
         collect (subseq ints 0 i)))))
;;   (map 'list (lambda (x) (subseq ints 0 (position x ints))) ints)))
;;   (map 'list (lambda (x) (reduce '+ x)) (map 'list (lambda (x) (subseq ints 0 (position x ints))) ints))))







;; From Stack Overflow :)
(defun get-duplicates (list &optional test)
  (let ((ht (make-hash-table :test (or test #'equal)))
        ret)
    (dolist (x list)
      (incf (gethash x ht 0)))
    (maphash (lambda (key value)
               (when (> value 1)
                 (push key ret)))
             ht)
    ret))
