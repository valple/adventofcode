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

;;reduce for given index
(defun oxyco2-red (bits i nr)
  (let* ((ilist (map 'list #'(lambda (x) (nth i x)) bits))
         (mcv (if (>= (count 1 ilist)
                      (/ (list-length ilist) 2)) nr (abs (- nr 1)))))
    (remove-if-not #'(lambda (x) (= mcv (nth i x)))
                   bits)))

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

;; Day 4
;;
;; P7

(defun read-bingo-game (file)
  (let* ((input (uiop:read-file-lines file))
         (numbers (car input))
         (inputs (cdr input)))
    (make-bingo-game
     :inputs (mapcar #'parse-integer (uiop:split-string numbers :separator ","))
     :boards (parse-bingos inputs nil nil))))

(defun trim-spaces-parse-ints (str)
  (mapcar #'parse-integer
          (remove-if #'uiop:emptyp (uiop:split-string str :separator " "))))

(defun parse-bingos (inputs current bingos)
  (let ((line (car inputs))
        (lines (cdr inputs)))
    (if inputs
        (if (uiop:emptyp line)
            (if current
                (parse-bingos lines nil
                              (append bingos (list (make-bingo current))))
                (parse-bingos lines nil bingos))
            (parse-bingos
             lines
             (append current
                     (list (trim-spaces-parse-ints line)))
             bingos))
        (if current
            (append bingos (list (make-bingo current)))
            bingos))))

(defstruct bingo-game
  inputs
  boards)

(defclass bingo ()
  ((board :accessor board
          :initarg :board
          :type int
          :documentation "5x5 matrix of numbers")
   (marked :accessor marked
           :initarg :marked
           :initform (make-array '(5 5) :initial-element -1)
           :type int
           :documentation "Empty 5x5 matrix that gets filled up"
           )))

(defun make-bingo (rows)
  (make-instance 'bingo
                 :board (make-array (list (list-length rows)
                                          (list-length (first rows))) :initial-contents rows)))

(defun in-2darray (board nr)
  (destructuring-bind (n m) (array-dimensions board)
    (loop for i below n do
          (loop for j below m do
                (if (eq (aref board i j) nr)
                    (return-from in-2darray (list i j)))))))

(defun ith-row (arr i)
  (let ((n (array-dimension arr 1)))
    (loop for j below n
          collect (aref arr i j) into row
          :finally (return (make-array n :initial-contents row)))))


(defun ith-col (arr i)
  (let ((n (array-dimension arr 0)))
    (loop for j below n
          collect (aref arr j i) into row
          :finally (return (make-array n :initial-contents row)))))

(defun entry-sum-2d (board)
   (destructuring-bind (n m) (array-dimensions board)
    (loop for i below n
          sum (loop for j below m
                sum (if (aref board i j) (aref board i j) 0)))))


(defmethod mark ((b bingo) (n integer))
  (let ((coord (in-2darray (board b) n)))
    (if coord
        (let ((i (first coord))
              (j (second coord)))
          (progn
            (setf (aref (board b) i j) nil)
            (setf (aref (marked b) i j) n))
            (if (and (find -1 (ith-row (marked b) i))
                    (find -1 (ith-col (marked b) j)))
                nil
                (* n (entry-sum-2d (board b))))))))

(defparameter *testgame* (read-bingo-game "testbingo.txt"))

(defun play-bingo (game)
  (let ((result nil))
    (loop for n in (bingo-game-inputs game) do
      (loop for b in (bingo-game-boards game) do
                (setf result (mark b n))
                when result
                  do (loop-finish)
                :finally (return result))
          when result
            do (loop-finish)
          :finally (return result))))

(defparameter *result7* (play-bingo (read-bingo-game "bingo.txt")))

;; P8

(defun play-loser-bingo (game)
  (let ((result nil))
    (loop for n in (bingo-game-inputs game) do
        (dolist (b (bingo-game-boards game))
          (progn
            (setf result (mark b n))
            (if result
                (if (> (list-length (bingo-game-boards game)) 1)
                    (setf (bingo-game-boards game) (remove b (bingo-game-boards game)))
                    (return-from play-loser-bingo result))))))))

(defparameter *result8* (play-loser-bingo (read-bingo-game "bingo.txt")))

;; Day 5
;;
;; P9

(defstruct point
  x
  y
  (marks 0))

(defstruct line
  start
  end)

(defun point-from-string (str)
  (let ((coord (uiop:split-string str :separator ",")))
    (make-point :x (parse-integer (first coord))
                :y (parse-integer (second coord)))))

(defun line-from-string (str)
  (let ((points (remove-if #'uiop:emptyp
                           (uiop:split-string str :separator " -> "))))
    (make-line :start (point-from-string (first points))
               :end (point-from-string (second points)))))

(defun read-hydro-lines (file)
  (let ((lines (uiop:read-file-lines file)))
    (mapcar #'line-from-string lines)))

(defun vert-points-on-line (aline)
  (let ((x1 (point-x (line-start aline)))
        (x2 (point-x (line-end aline)))
        (y1 (point-y (line-start aline)))
        (y2 (point-y (line-end aline))))
    (cond
      ((= x1 x2) (loop for i from (min y1 y2) to (max y1 y2)
                       collect (make-point :x x1 :y i)))
      ((= y1 y2) (loop for i from (min x1 x2) to (max x1 x2)
                       collect (make-point :x i :y y1)))
      (t nil))))

(defun hydrothermal-vents (linefunction lines)
  (let ((marked (make-array '(1000 1000) :initial-element -1)))
    (loop for line in lines do
      (loop for apoint in (funcall linefunction line) do
        (setf (aref marked (point-x apoint) (point-y apoint))
              (+ 1 (aref marked (point-x apoint) (point-y apoint)))))
      :finally (return
                 (destructuring-bind (n m) (array-dimensions marked)
                   (loop for i below n
                         sum (loop for j below m
                                   count (> (aref marked i j)))))))))


(defparameter *result9* (hydrothermal-vents #'vert-points-on-line
                                            (read-hydro-lines "hydrothermal.txt")))
;; P10

(defun points-on-line (aline)
  (let ((x1 (point-x (line-start aline)))
        (x2 (point-x (line-end aline)))
        (y1 (point-y (line-start aline)))
        (y2 (point-y (line-end aline))))
    (cond
      ((= x1 x2) (loop for i from (min y1 y2) to (max y1 y2)
                       collect (make-point :x x1 :y i)))
      ((= y1 y2) (loop for i from (min x1 x2) to (max x1 x2)
                       collect (make-point :x i :y y1)))
      ((= (abs (- y2 y1)) (abs (- x2 x1)))
       (let ((x (- x2 x1)) (y (- y2 y1)))
         (loop for i from 0 to (abs x)
             collect (make-point :x (+ x1 (* i (/ x (abs x))))
                                 :y (+ y1 (* i (/ y (abs x))))))))
      (t nil))))

(defparameter *result10* (hydrothermal-vents #'points-on-line
                                             (read-hydro-lines "hydrothermal.txt")))

;; Day 6
;;
;; P11

;; Fancy class method
;; super slow of course
(defclass lanternfish ()
  ((age :accessor age
        :initarg :age
        :initform 8)))

(defun make-lanternfish (&optional (current-age 8))
  (make-instance 'lanternfish :age current-age))

(defmethod pass-day ((l lanternfish))
  (let ((x (age l)))
    (if (= x 0)
        (progn
          (setf (age l) 6)
          (list l (make-lanternfish)))
         (progn
          (setf (age l) (- x 1))
          (list l)))))



(defclass lantern-model ()
  ((day :accessor day
        :initarg :day
        :initform 0)
   (fishes :accessor fishes
           :initarg :fishes
           :initform nil)))

(defun make-lantern-model (&optional (fishes nil) (day 0))
  (make-instance 'lantern-model :day day
                                :fishes (map 'list #'make-lanternfish fishes)))

(defun read-fishes (file)
  (mapcar #'(lambda (str) (parse-integer (string-trim " " str)))
          (uiop:split-string (uiop:read-file-string file) :separator ",")))

(defmethod pass-day ((lm lantern-model))
  (let ((d (day lm)))
    (progn
      (setf (day lm) (+ d 1))
      (setf (fishes lm)
            (loop for fish in (fishes lm)
                 nconc (pass-day fish)))
      (list-length (fishes lm)))))

(defun run-fish-model (duration fishes)
  (let ((lm (make-lantern-model fishes)))
    (loop repeat duration
          collect (pass-day lm) into nr-fishes
          :finally (return (first (last nr-fishes))))))

(defparameter *result11* (run-fish-model 80 (read-fishes "fishes.txt")))

;; P2

;; Quicker method for nr 1 & 2
;;fucking hell gimme good package
;; no error checks
(defun matrix-mult (a b)
  (let* ((n (array-dimension a 0))
         (m (array-dimension b 1))
         (p (array-dimension a 1))
         (res (make-array (list n m) :element-type 'integer :initial-element 0)))
    (loop for i below n do
          (loop for j below m do
            (setf (aref res i j)
                  (loop for k below p
                        sum (* (aref a i k) (aref b k j)))))
          :finally (return res))))

(defparameter *id-matrix*
  (make-array '(9 9) :element-type 'integer
                    :initial-contents (loop for i below 9 collect
                                            (loop for j below 9 collect
                                                  (if (= i j) 1 0)))))
(defun matrix-power (a n)
  (if (= n 0)
      *id-matrix*
      (if (= n 1)
          a
          (matrix-mult
           (matrix-power a (- n 1))
           a))))

(defparameter *matrix-1y*
  (make-array '(9 9) :element-type 'integer
                     :initial-contents (loop for i below 9 collect
                                             (loop for j below 9 collect
                                                   (if (= (mod (+ 1 i) 9) j)
                                                       1
                                                       (if (and (= i 6) (= j 0))
                                                           1
                                                           0))))))

(defun run-fast-fish-model (duration fishes)
  (let ((fisharray (make-array '(9 1) :element-type 'integer :initial-contents
                               (loop for i below 9
                                     collect (list (max (count i fishes) 0))))))
    (entry-sum-2d (matrix-mult (matrix-power *matrix-1y* duration)
                               fisharray))))

(defparameter *result12* (run-fast-fish-model 256 (read-fishes "fishes.txt")))
