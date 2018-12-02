;; from stack overflow
(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defun count (a L)
  (cond
    ((null L) 0)
    ((equal a (car L)) (+ 1 (count a (cdr L))))
    (t (count a (cdr L)))))

;; maps counts to list
;; (a a c b c c) => (2 2 3 1 3 3)
(defun map-count (listicle)
  (map 'list (lambda (x) (count x listicle)) listicle))

(defun advent-two-one (filename)
  (let ((listicle (get-file filename)))
    (*
     (count '2 (map 'list (lambda (x) (find '2 x)) (map 'list (lambda (x) (map-count x)) listicle)))
     (count '3 (map 'list (lambda (x) (find '3 x)) (map 'list (lambda (x) (map-count x)) listicle))))))

(defun levenshtein (a b)
  (let* ((la  (length a))
	       (lb  (length b))
	       (rec (make-array (list (1+ la) (1+ lb)) :initial-element nil)))

    (defun leven (x y)
      (cond
	      ((zerop x) y)
	      ((zerop y) x)
	      ((aref rec x y) (aref rec x y))
	      (t (setf (aref rec x y)
		             (+ (if (char= (char a (- la x)) (char b (- lb y))) 0 1)
		                (min (leven (1- x) y)
			                   (leven x (1- y))
			                   (leven (1- x) (1- y))))))))
    (leven la lb)))

(defun advent-two-two (filename)
  (let ((listicle (get-file filename)))
    (defun advent (a L)
      (cond
        ((null L) 0)
        ((equal 1 (levenshtein a (car L))) (format t "Found: ~A~%Other: ~A~%-----~%" a (car L)))
        (t (advent a (cdr L)))))
    (loop for i from 0 to (length listicle) do
         (map 'list (lambda (x) (advent x listicle)) listicle))))

;; (defun advent-two-two-fuck (filename)
;;   (let ((listicle (get-file filename)))
;;     (loop for i from 0 to (length listicle) do
;;          (map 'list (lambda (x) (levenshtein x (elt listicle i)))
;;               (loop for i from 0 to (length listicle)
;;                  collect listicle)))))
