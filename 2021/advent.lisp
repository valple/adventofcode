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

;; Day 3
;;
;; P5

(defun read-bits ()
  (let ((bits (uiop:read-file-lines "bits.txt")))
    (mapcar
     #'(lambda (y)
         (loop for i across y
               collect (digit-char-p i)))
    bits)
    ))

(defun int-from-bitlist (binlist)
  (parse-integer (map 'string #'digit-char binlist) :radix 2))

;; Sum up the 1's
;; Divide by length of input
;; Round nearest int (0 or 1)
;; reconvert to binary number
;; use not on binary number
;; multiply the two
(defun power-consumption (bits)
  (let* ((gamma (mapcar #'(lambda (x) (round (/ x (list-length bits))))
               (reduce #'(lambda (x y) (mapcar #'+ x y)) bits)))
        (eps (map 'list #'(lambda (x) (abs (- x 1))) gamma)))
    (* (int-from-bitlist gamma) (int-from-bitlist eps))))

(defparameter *result5* (power-consumption (read-bits)))

;; P6
;;

(defun oxyco2-red (bits i nr)
  (let* ((ilist (map 'list #'(lambda (x) (nth i x)) bits))
         (mcv (if (>= (count 1 ilist) (/ (list-length ilist) 2)) nr (abs (- nr 1)))))
    (remove-if-not #'(lambda (x) (= mcv (nth i x))) bits)))

(defun oxyco2 (bits nr)
  (let ((res bits))
    (progn (loop for i below (list-length (first bits))
                 until (= 1 (list-length res))
                 do (setf res (oxyco2-red res i nr)))
           res)))

(defun life-support (bits)
  (* (int-from-bitlist (first (oxyco2 bits 1)))
     (int-from-bitlist (first (oxyco2 bits 0)))))

(defparameter *result6* (life-support (read-bits)))
