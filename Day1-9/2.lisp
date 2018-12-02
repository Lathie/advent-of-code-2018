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
