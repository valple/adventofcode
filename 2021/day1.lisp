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

;; Day 2
;;
;; P1

(defun read-commands ()
  (let ((x (uiop:read-file-lines "d2commands.txt")))
    (mapcar #'uiop:split-string x)))

(defvar *commands* (read-commands))

(defun parse-commands (commands)
  (let ((depth 0) (horiz 0))
    (dolist (k commands)
      (let ((str (car k)) (nr (parse-integer (car (cdr k)))))
        (cond
          ((string= str "forward")
           (setf horiz (+ horiz nr)))
          ((string= str "down")
           (setf depth (+ depth nr)))
          ((string= str "up")
           (setf depth (- depth nr))))))
    (* depth horiz)))

(defvar *result3* (parse-commands *commands*))

;; P4
;;with aim
(defun parse-commands2 (commands)
  (let ((depth 0) (horiz 0) (aim 0))
    (dolist (k commands)
      (let ((str (car k)) (nr (parse-integer (car (cdr k)))))
        (cond
          ((string= str "forward")
           (progn (setf horiz (+ horiz nr))
                  (setf depth (+ depth (* aim nr)))))
          ((string= str "down")
           (setf aim (+ aim nr)))
          ((string= str "up")
           (setf aim (- aim nr))))))
    (* depth horiz)))

(defvar *result4* (parse-commands2 *commands*))
