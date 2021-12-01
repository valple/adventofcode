;; Day 1
;; P1

;; count differences
(defun count-depth-increases (depths)
  (count-if #'(lambda (x) (< (car x) (cdr x))) (pairlis (butlast depths) (cdr depths))))

;;read file and convert to integer
(defun read-depths ()
  (let ((x (uiop:read-file-lines "depths.txt")))
    (mapcar #'parse-integer x)))

(defvar *depths* (read-depths))

(defvar *result1* (count-depth-increases *depths*))

;; P2

;; create 3-sum vector
(defun create-3sum-depths (depths)
  (let ((x (butlast (butlast depths)))
        (y (butlast (cdr depths)))
        (z (cdr (cdr depths))))
    (mapcar #'+ x y z)))

(defvar *result2* (count-depth-increases (creats-3sum-depths *depths*)))
