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

;; Day 7
;;
;; P1


(defun read-crabs (file)
  (sort
   (map 'list #'parse-integer
       (uiop:split-string (uiop:read-file-string file) :separator ","))
   #'<))

(defun fuel-calc (crabs)
  (let* ((len (list-length crabs))
         (median (nth (floor (/ len 2)) crabs)))
    (loop for i in crabs
          sum (abs (- i median)))))

(defparameter *result13* (fuel-calc (read-crabs "crabfuel.txt")))

;; P 2
;;
;;Too much headache today to think but it should be somewhere between the average and mean
(defun more-fuel-calc (crabs)
  (let* ((len (list-length crabs))
         (median (nth (floor (/ len 2)) crabs))
         (mean  (round (/ (loop for i in crabs
                      sum i) len))))
    (loop for j from (min mean median) to (max mean median)
          collect
          (loop for i in crabs
                sum (/ (* (abs (- i j)) (+ 1 (abs (- i j)))) 2)) into sums
          :finally (return (apply #'min sums)))))

(defparameter *result14* (more-fuel-calc (read-crabs "crabfuel.txt")))

;; Day 8
;;
;; P1

(defun read-segments (file)
  (map 'list #'(lambda (x) (uiop:split-string x :separator "|"))
       (uiop:read-file-lines file)))

(defun some-counting (segments)
  (loop for i in segments
        sum (count-if #'(lambda (x) (or
                                     (= (length x) 2)
                                     (= (length x) 3)
                                     (= (length x) 4)
                                     (= (length x) 7)))
                      (map 'list #'(lambda (x) (string-trim " " x))
                           (uiop:split-string (second i) :separator " ")))))

(defparameter *result15* (some-counting (read-segments "segmentsearch.txt")))

;; P2
;;

(defvar *nr-codes* '((t t t nil t t t)
                    (nil nil t nil nil t nil)
                    (t nil t t t nil t)
                    (t nil t t nil t t)
                    (nil t t t nil t nil)
                    (t t nil t nil t t)
                    (t t nil t t t t)
                    (t nil t nil nil t nil)
                    (t t t t t t t)
                    (t t t t nil t t)))

(defun make-possiblities ()
  (loop for i below 7
        collect '(#\a #\b #\c #\d #\e #\f #\g)))

;; doesn't work for strings without modification!
(defun list-rem-if-in (l1 l2)
  (remove-if #'(lambda (x) (member x l1)) l2))

(defun list-rem-if-not-in (l1 l2)
  (remove-if-not #'(lambda (x) (member x l1)) l2))

(defun list-is-member (l1 l2)
  (loop for x in l1
        if (member x l2) collect x))

(defun deduce-code (strings)
  (let ((possible (make-possiblities))
        (str-parted (partition-by-stringlen strings)))
    (loop for part in str-parted do
      (cond
        ((= (length (first part)) 2)
         (loop for i from 0 to 6
               collect (if (nth i (nth 1 *nr-codes*))
                           (list-rem-if-not-in (coerce (first part) 'list) (nth i possible))
                           (list-rem-if-in (coerce (first part) 'list) (nth i possible)))
                 into newpossible
               :finally (setf possible newpossible)))
        ((= (length (first part)) 3)
         (loop for i from 0 to 6
               collect (if (nth i (nth 7 *nr-codes*))
                           (list-rem-if-not-in (coerce (first part) 'list) (nth i possible))
                           (list-rem-if-in (coerce (first part) 'list) (nth i possible)))
                 into newpossible
               :finally (setf possible newpossible)))
        ((= (length (first part)) 4)
         (loop for i from 0 to 6
               collect (if (nth i (nth 4 *nr-codes*))
                           (list-rem-if-not-in (coerce (first part) 'list) (nth i possible))
                           (list-rem-if-in (coerce (first part) 'list) (nth i possible)))
                 into newpossible
               :finally (setf possible newpossible)))
        ((= (length (first part)) 5)
         (progn
           (destructuring-bind (x y) (nth 4 possible)
             (loop for str in part do
               (let ((charlist (coerce str 'list)))
                 (if (member x charlist)
                     (if (member y charlist)
                         (progn
                           (setf (nth 2 possible) (list-is-member charlist (nth 2 possible)))
                           (setf (nth 5 possible) (list-rem-if-in (nth 2 possible) (nth 5 possible))))
                         (progn
                           (setf (nth 6 possible) (list x))
                           (setf (nth 4 possible) (list-rem-if-in (nth 6 possible) (nth 4 possible)))))
                     (progn
                       (setf (nth 6 possible) (list y))
                       (setf (nth 4 possible) (list-rem-if-in (nth 6 possible) (nth 4 possible))))))))
            (destructuring-bind (x y) (nth 1 possible)
             (loop for str in part do
               (let ((charlist (coerce str 'list)))
                 (if (member x charlist)
                     (if (member y charlist)
                         nil
                         (progn
                           (setf (nth 3 possible) (list x))
                           (setf (nth 1 possible) (list y))))
                     (progn
                           (setf (nth 3 possible) (list y))
                           (setf (nth 1 possible) (list x)))))))))
         (t nil))
        :finally (return possible))))

(defun partition-by-stringlen (strlist)
  (let ((maxlist strlist))
    (loop for i from 1 to (reduce #'max (map 'list #'length maxlist))
        collect (loop for str in strlist
                      if (= (length str) i) collect str))))

(defun code-to-int-str (str possible)
  (loop for chr in (coerce str 'list)
        collect (loop for i from 0 to 6
                      when (char= (first (nth i possible)) chr)
                        return i) into pos
        :finally (return
                   (loop for i from 0 to 9
                         when (loop for j from 0 to 6
                                    when (or (and (not (nth j (nth i *nr-codes*)))
                                                  (member j pos))
                                             (and (not (member j pos))
                                                  (nth j (nth i *nr-codes*))))
                                      return nil
                                    finally (return t))
                           return i))))

(defun clean-linepart(linepart)
  (remove-if #'uiop:emptyp (uiop:split-string linepart :separator " ")))


(defun part2-counting (segments)
  (reduce #'+
   (loop for line in segments
         collect
         (destructuring-bind (l1 l2) line
           (let ((possible (deduce-code (clean-linepart l1))))
             (loop for str in (clean-linepart l2)
                   collect (code-to-int-str str possible) into nrs
                   finally (return (parse-integer
                                    (reduce #'(lambda (x y)
                                               (concatenate 'string x y))
                                           (map 'list #'write-to-string nrs))))))))))

(defparameter *result16* (part2-counting (read-segments "segmentsearch.txt")))

;; Day 9
;;
;; P1

(defun read-smoke (file)
  (let ((heights (uiop:read-file-lines file)))
    (make-array (list (list-length heights) (length (first heights)))
                :initial-contents
                (map 'list #'(lambda (x) (map 'list #'parse-integer
                                (map 'list #'string
                                     (coerce x 'list))))
                      heights))))

(defun low-than-elem (i j mat n m)
  (destructuring-bind (x y) (array-dimensions mat)
    (if (or (< n 0) (< m 0) (>= n x) (>= m y))
        t
        (< (aref mat i j) (aref mat n m)))))

(defun make-neighbors (i j)
  (list (list (- i 1) j)
        (list (+ i 1) j)
        (list i (+ j 1))
        (list i (- j 1))))

(defun low-than-neighbors (i j mat)
      (loop for n in (make-neighbors i j)
            when (not (low-than-elem i j mat (first n) (second n)))
               do (return-from low-than-neighbors 0)
          finally (return (+ 1 (aref mat i j)))))

(defun count-basins (heights)
  (destructuring-bind (x y) (array-dimensions heights)
    (loop for i from 0 below x
          sum (loop for j from 0 below y
                    sum (low-than-neighbors i j heights)))))

(defparameter *result17* (count-basins (read-smoke "smokebasin.txt")))

;; P2
;;
(defun between-nr (x a b)
  (and (< x b) (>= x a)))

(defun connected-coord (outer res heights)
  (destructuring-bind (x y) (array-dimensions heights)
    (if outer
        (let ((elem (car outer))
              (nouter (cdr outer)))
          (let ((elem1 (first elem))
                (elem2 (second elem)))
            (if (and (between-nr elem1 0 x)
                     (between-nr elem2 0 y))
                (if (< (aref heights elem1 elem2) 9)
                    (loop for n in (make-neighbors elem1 elem2)
                          if (and (not (member n res :test #'equal))
                                  (between-nr (first n) 0 x)
                                  (between-nr (second n) 0 y))
                            collect n into news
                          finally (return
                                    (connected-coord (append nouter news)
                                                     (append res (list elem))
                                                     heights)))
                    (connected-coord nouter res heights))
                (connected-coord nouter res heights))))
        (remove-duplicates res :test #'equal))))

(defun find-lpts (heights)
  (destructuring-bind (x y) (array-dimensions heights)
    (loop for i from 0 below x
          collect (loop for j from 0 below y
                        if (> (low-than-neighbors i j heights) 0)
                          collect (list i j)) into lpts
          finally (return (reduce #'append lpts)))))

(defun size-of-basin (lpt heights)
  (list-length (connected-coord (list lpt) nil heights)))

(defun sizes-of-basins (heights)
  (sort
    (map 'list #'(lambda (x) (size-of-basin x heights)) (find-lpts heights))
    #'>))

(defun prod-largest-three (heights)
  (let ((sizes (sizes-of-basins heights)))
    (* (first sizes) (second sizes) (third sizes))))

(defparameter *result18* (prod-largest-three (read-smoke "smokebasin.txt")))

;; Day 10
;;
;; P1

(defun read-syntax (file)
  (map 'list #'(lambda (x) (coerce x 'list))
       (uiop:read-file-lines file)))

(defun value-chr (chr)
  (cond
    ((char= chr #\)) 3)
    ((char= chr #\]) 57)
    ((char= chr #\}) 1197)
    ((char= chr #\>) 25137)
    (t 0)))

(defparameter *open-brackets* '(#\( #\[ #\{ #\<))
(defparameter *closed-brackets* '(#\) #\] #\} #\>))

(defun bracket-closer (chr)
  (nth (position chr *open-brackets* :test #'char=) *closed-brackets*))

(defun corrupted-score (syntax-start syntax-end)
  (if syntax-end
      (let ((chr (car syntax-end)))
        (if (member chr *open-brackets* :test #'char=)
            (is-corrupted (append syntax-start (list chr)) (cdr syntax-end))
            (if syntax-start
                (if (char= chr (bracket-closer (car (last syntax-start))))
                    (is-corrupted (reverse (cdr (reverse syntax-start))) (cdr syntax-end))
                    (value-chr chr))
                0)))
      (if syntax-start
          0
          0)))

(defun sum-syntax-errors (syntax-lines)
  (loop for line in syntax-lines
        sum (corrupted-score nil line)))

(defparameter *result19* (sum-syntax-errors (read-syntax "syntax.txt")))

;; P2
;;

(defun value-chr2 (chr)
  (+ 1 (position chr *closed-brackets* :test #'char=)))

(defun is-incomplete (syntax-start syntax-end)
  (if syntax-end
     (let ((chr (car syntax-end)))
       (if (member chr *open-brackets* :test #'char=)
           (is-incomplete (append syntax-start (list chr)) (cdr syntax-end))
           (if syntax-start
               (if (char= chr (bracket-closer (car (last syntax-start))))
                   (is-incomplete (reverse (cdr (reverse syntax-start))) (cdr syntax-end))
                   nil)
               nil)))
     (if syntax-start
         (reverse syntax-start)
         nil)))

(defun incomplete? (line)
  (is-incomplete nil line))

(defun score-incomplete (chrs)
  (loop with score = 0 for chr in chrs do
             (setf score (+ (* 5 score) (value-chr2 (bracket-closer chr))))
        finally (return score)))

(defun median (l)
  (nth (floor (/ (list-length l) 2)) l))

(defun score-incompletes (syntax-lines)
  (median
   (sort (map 'list #'score-incomplete
             (remove nil (map 'list #'incomplete? syntax-lines)))
        #'<)))

(defparameter *result20* (score-incompletes (read-syntax "syntax.txt")))

;; Day 11
;;
;; P1

(defun read-dumbos (file)
  (let ((matelem
          (map 'list #'(lambda (x) (map 'list #'digit-char-p (coerce x 'list)))
               (uiop:read-file-lines file))))
    (make-array (list (list-length matelem) (list-length (first matelem)))
                :initial-contents matelem)))


(defun make-diag-neighbors (i j)
  (list (list (- i 1) j)
        (list (+ i 1) j)
        (list i (+ j 1))
        (list i (- j 1))
        (list (+ i 1) (+ j 1))
        (list (+ i 1) (- j 1))
        (list (- i 1) (+ j 1))
        (list (- i 1) (- j 1))))


(defun mat+c-mod (mat c)
  (destructuring-bind (n m) (array-dimensions mat)
    (loop for i from 0 below n do
          (loop for j from 0 below m do
            (let ((z (aref mat i j)))
              (setf (aref mat i j) (mod (+ c z) 10))))
          finally (return mat))))

;; side-effects
(defun flash (mat i j)
  (let ((nmat mat))
    (progn
      (setf (aref nmat i j) nil)
      (destructuring-bind (n m) (array-dimensions nmat)
        (loop for neighbor in (make-diag-neighbors i j) do
          (let ((a (first neighbor))
                 (b (second neighbor)))
            (if (and (between-nr a 0 n) (between-nr b 0 m))
                (let ((z (aref nmat a b)))
                  (unless (or (eq 0 z) (not z))
                    (setf (aref nmat a b) (mod (+ 1 z) 10))))))
              finally (return nmat))))))

(defun traverse-energies (mat)
  (destructuring-bind (n m) (array-dimensions mat)
      (loop for i from 0 below n do
            (loop for j from 0 below m do
                  (let ((z (aref mat i j)))
                    (when (eq z 0)
                      (return-from traverse-energies (traverse-energies (flash mat i j))))))
            finally (return mat))))

(defun cycle (mat)
  (let ((nmat (traverse-energies (mat+c-mod mat 1)))
        (flashes 0))
    (destructuring-bind (n m) (array-dimensions nmat)
      (loop for i from 0 below n do
            (loop for j from 0 below m do
                  (if (not (aref nmat i j))
                      (progn
                        (setf (aref nmat i j) 0)
                        (setf flashes (+ 1 flashes)))))
            finally (return (cons nmat flashes))))))

(defun n-cycle (mat n flashes)
  (if (= 0 n)
      flashes
      (let ((nmat (cycle mat)))
        (n-cycle (car nmat) (- n 1) (+ flashes (cdr nmat))))))

(defparameter *result21* (n-cycle (read-dumbos "dumbos.txt") 100 0))


(defun n-cycle-break (mat)
  (destructuring-bind (n m) (array-dimensions mat)
    (let ((total (* n m))
          (flashes 0)
          (run 0)
          (nmat nil))
     (loop while (/= total flashes) do
       (progn
         (setf nmat (cycle mat))
         (setf mat (car nmat))
         (setf flashes (cdr nmat))
         (setf run (+ 1 run)))
       finally (return run)))))

(defparameter *result22* (n-cycle-break (read-dumbos "dumbos.txt")))

;; Day 12
;;
;; P1

(defun read-paths (file)
  (map 'list #'(lambda (x) (uiop:split-string x :separator "-"))
       (uiop:read-file-lines file)))

(defun make-caves (paths)
  (let ((cave-table (make-hash-table :test 'equal)))
    (loop for str in (loop for path in paths
                           append (list (first path) (second path)) into total
                           finally (return (remove-duplicates total :test #'string=))) do
                        (setf (gethash str cave-table) nil)
          finally (return cave-table))))

(defun add-paths (paths cave-table)
  (loop for path in paths do
    (let* ((p1 (first path))
           (p2 (second path))
           (curr1 (gethash p1 cave-table))
           (curr2 (gethash p2 cave-table)))
      (progn
        (setf (gethash p1 cave-table) (append curr1 (list p2)))
        (setf (gethash p2 cave-table) (append curr2 (list p1)))))
    finally (return cave-table)))

(defun possible-paths (incomplete-paths complete-paths cave-table)
  (if incomplete-paths
      (let* ((curr-path (car incomplete-paths))
             (incomp-paths (cdr incomplete-paths))
             (last-cave (car (last curr-path)))
             (options (gethash last-cave cave-table)))
        (loop for opt in (remove-if #'(lambda (x) (and (lower-case-p (char x 0))
                                                       (member x curr-path :test #'string=)))
                                    options)
              if (string= opt "end") collect (append curr-path (list opt)) into comps
                else collect (append curr-path (list opt)) into incomps
              finally (return (possible-paths (append incomp-paths incomps)
                                              (+ complete-paths (list-length comps)) cave-table))))
      complete-paths))

(defun calc-possible-paths (file)
  (let* ((paths (read-paths file))
         (cave-table (add-paths paths (make-caves paths))))
    (possible-paths '(("start")) 0 cave-table)))

(defparameter *result23* (calc-possible-paths "paths.txt"))

;; P2
;;
(defun add-possible-second (path elem)
  (if (upper-case-p (char elem 0))
      (append path (list elem))
      (if (member elem path :test #'string=)
          (append (list t) (cdr path) (list elem))
          (append path (list elem)))))

(defun possible-paths2 (incomplete-paths complete-paths cave-table)
  (if incomplete-paths
      (let* ((curr-path (car incomplete-paths))
             (incomp-paths (cdr incomplete-paths))
             (last-cave (car (last curr-path)))
             (options (gethash last-cave cave-table)))
        (loop for opt in (remove-if #'(lambda (x) (or (and (lower-case-p (char x 0))
                                                           (> (count x curr-path :test #'string=)
                                                              (if (car curr-path) 0 1)))
                                                      (string= x "start")))
                                    options)
              if (string= opt "end") count opt into comps
                else collect (add-possible-second curr-path opt) into incomps
              finally (return (possible-paths2 (append incomps incomp-paths)
                                              (+ complete-paths comps) cave-table))))
      complete-paths))

(defun possible-paths3 (cave-table)
  (let ((incomplete-paths '((nil "start")))
        (complete-paths 0))
    (loop while incomplete-paths do
      (let* ((curr-path (car incomplete-paths))
             (incomp-paths (cdr incomplete-paths))
             (last-cave (car (last curr-path)))
             (options (gethash last-cave cave-table)))
        (loop for opt in (remove-if #'(lambda (x) (or (and (lower-case-p (char x 0))
                                                           (> (count x curr-path :test #'string=)
                                                              (if (car curr-path) 0 1)))
                                                      (string= x "start")))
                                    options)
              if (string= opt "end") count opt into comps
                else collect (add-possible-second curr-path opt) into incomps
              finally (progn
                        (setf incomplete-paths (append incomps incomp-paths))
                        (setf complete-paths (+ complete-paths comps)))))
      :finally (return complete-paths))))


(defun calc-possible-paths2 (file)
  (let* ((paths (read-paths file))
         (cave-table (add-paths paths (make-caves paths))))
    (possible-paths2 '((nil "start")) 0 cave-table)))


(defun calc-possible-paths3 (file)
  (let* ((paths (read-paths file))
         (cave-table (add-paths paths (make-caves paths))))
    (possible-paths3 cave-table)))


(defparameter *result24* (calc-possible-paths3 "paths.txt"))

;; Day 13
;;
;; P1

(defun read-folds (file)
  (let* ((folds (uiop:read-file-lines file))
         (cutoff (position "" folds :test #'string=)))
    (cons (map 'list #'(lambda (x) (map 'list #'parse-integer
                                        (uiop:split-string x :separator ",")))
               (subseq folds 0 cutoff))
          (subseq folds (+ 1 cutoff)))))

;; so inefficient
(defun make-fold-matrix (folds)
  (destructuring-bind (x y) (loop for (i j) in folds
                                  maximize i into mx
                                  maximize j into my
                                  finally (return (list (+ 1 mx) (+ 1 my))))
    (let ((fmatrix (make-array (list y x) :initial-element nil)))
      (loop for (y x) in folds do
            (setf (aref fmatrix x y) t)
            finally (return fmatrix)))))

;;mirror along m
(defun mirror-up (m y)
  (+ m (- m y)))

;; horiz fold - fold along x
;; otheriwse fold along y
(defun fold-matrix (mat nr horiz)
  (let* ((x (array-dimension mat 0))
         (y (array-dimension mat 1))
         (dims (if horiz (list x nr)
                         (list nr y)))
         (fld-index (if horiz (min (- y 1 nr) nr)
                              (min (- x 1 nr) nr))))
    (loop for i below (first dims)
          collect (loop for j below (second dims)
                        collect (or (aref mat i j)
                                    (if horiz
                                        (if (between-nr j (- nr fld-index) nr)
                                            (aref mat i (mirror-up nr j))
                                            nil)
                                        (if (between-nr i (- nr fld-index) nr)
                                            (aref mat (mirror-up nr i) j)
                                            nil)))) into nmat
          finally (return (make-array dims :initial-contents nmat)))))

(defun count-dots (mat)
  (loop for i below (array-dimension mat 0)
        sum (loop for j below (array-dimension mat 1)
                  if (aref mat i j) count 1)))

(defparameter *result25* (count-dots
                          (fold-matrix
                           (make-fold-matrix
                            (car (read-folds "fold.txt")))
                           655 t)))

(defun decode-instruction (line)
  (apply #'(lambda (x y) (list (find "x"  x :test #'string=)
                             (parse-integer y)))
         (uiop:split-string line :separator "=")))

(defun decode-instructions (instructions)
  (map 'list #'decode-instruction instructions))

(defun do-all-folds (mat instructions)
  (if instructions
      (do-all-folds
          (fold-matrix mat
                                 (second (first instructions))
                                 (first (first instructions)))
        (cdr instructions))
      mat))

(defun fold-input (input)
  (let ((mat (make-fold-matrix (car input)))
        (instr (decode-instructions (cdr input))))
    (do-all-folds mat instr)))

(defun pretty-print-mat (mat)
  (loop for i below (array-dimension mat 0)
        collect (loop for j below (array-dimension mat 1) do
                     (format t "~c " (if (aref mat i j) #\# #\.))
                      finally (format t "~%"))))

(defparameter *result26* (fold-input (read-folds "fold.txt")))

;; Day 14
;;
;; P1

(defun read-polymer (file)
  (let* ((polys (uiop:read-file-lines file))
         (cutoff (position "" polys :test #'string=)))
    (cons (subseq polys 0 cutoff)
          (map 'list #'(lambda (x)
                         (remove-if #'uiop:emptyp
                                              (uiop:split-string x :separator " -> ")))
               (subseq polys (+ 1 cutoff))))))

(defun apply-rule (charpair rule)
  (if (string= charpair (first rule))
      (second rule)
      ""))

(defun pair-concat-strings (str1 str2)
  (concatenate 'string str1 str2))

(defun apply-all-rules (charpair rules)
  (loop for rule in rules
        collect (apply-rule charpair rule) into stringlist
        finally (return (concatenate 'string
                                     (subseq charpair 0 1)
                                     (reduce #'pair-concat-strings stringlist)))))

(defun one-step-apply-rules (word rules)
  (loop for i below (- (length word) 1)
        collect (apply-all-rules (subseq word i (+ 2 i)) rules) into new-word
        finally (return (concatenate 'string
                                     (reduce #'pair-concat-strings new-word)
                                     (subseq (reverse word) 0 1)))))

(defun n-steps-apply (word rules n)
  (if (= 0 n)
      word
      (n-steps-apply (one-step-apply-rules word rules) rules (- n 1))))

(defun chr-freq-rec (word freq)
  (if (uiop:emptyp word)
      freq
      (let ((chr (subseq word 0 1)))
        (chr-freq-rec (remove chr word :test #'string=)
                      (append freq (list (list chr
                                               (count chr word :test #'string=))))))))

(defun char-freq (word)
  (chr-freq-rec word nil))

(defun sort-freq (freq-list)
  (sort freq-list #'(lambda (x y) (if (< (second x) (second y)) t nil))))

(defun solve-word (input)
  (let ((word (car (car input)))
        (rules (cdr input)))
    (n-steps-apply word rules 10)))

(defun calc-most-least (word)
  (let ((sorted-freq (sort-freq (char-freq word))))
    (- (second (car (last sorted-freq)))
       (second (first sorted-freq)))))

(defun calc-part1-polymer (input)
   (calc-most-least (solve-word input)))

(defparameter *result27* (calc-part1-polymer (read-polymer "polymer.txt")))

;; P2

(defun apply-rule (charpair rules)
  (loop for rule in rules
        when (string= (first rule) charpair)
          return (list (concatenate 'string (subseq charpair 0 1) (second rule))
                       (concatenate 'string  (second rule) (subseq charpair 1 2)))
        finally (return (list charpair))))

(defparameter *word* (car (car (read-polymer "polymer-test.txt"))))
(defparameter *rules* (cdr (read-polymer "polymer-test.txt")))

(defparameter *char-hash* (make-hash-table))

(defun initial-table (word)
  (let ((htable (make-hash-table :test 'equal)))
    (loop for i below (- (length word) 1) do
      (setf (gethash (subseq word i (+ i 2)) htable)
            (plusif (gethash (subseq word i (+ i 2)) htable) 1))
    finally (return htable))))

;; well that's gonna be inefficient as fuck too
(defun apply-rules-once (htable rules)
  (let ((new-htable (make-hash-table :test 'equal)))
    (loop for k being each hash-key of htable
          using (hash-value val)
          do (loop for i in (apply-rule k rules)
                   do
                      (setf (gethash i new-htable)
                            (plusif (gethash i new-htable) val)))
             finally (return new-htable))))

(defun plusif (a b)
  (if a
      (+ a b)
      b))

(defun apply-rules-n-times (htable rules n)
  (if (= n 0)
      htable
      (apply-rules-n-times (apply-rules-once htable rules) rules (- n 1))))

(defun calc-count-table (htable)
  (let ((count-table (make-hash-table :test 'equal)))
    (loop for k being each hash-key of htable
          using (hash-value val)
          do
             (loop for c across k
                   do
                (let ((val2 (gethash (string c) count-table)))
                  (setf (gethash (string c) count-table)
                        (plusif val2 val))))
          finally (return
                    (loop for k being each hash-key of count-table
                          using (hash-value val) do
                            (if (or (string= k "P") (string= k "O"))
                              (setf (gethash k count-table) (+ (/ (- val 1) 2) 1))
                              (setf (gethash k count-table) (/ val 2)))
                          finally (return count-table))))))

(defun print-table (htable)
  (loop for k being each hash-key of htable
        using (hash-value val)
        do (format t "~s : ~s ~%" k val)))

(defparameter *word* (car (car (read-polymer "polymer-test.txt"))))
(defparameter *rules* (cdr (read-polymer "polymer-test.txt")))

(defun part2-calc-count-table (file n)
  (let* ((input (read-polymer file))
         (word (car (car input)))
         (rules (cdr input)))
    (calc-count-table
     (apply-rules-n-times (initial-table word) rules n))))

(defun calc-max-min (count-table)
  (loop for k being each hash-key of count-table
        using (hash-value val)
        maximize val into maxi
        minimize val into mini
        finally (return (- maxi mini))))

(defparameter *result28* (calc-max-min
                          (part2-calc-count-table "polymer.txt" 40)))

;; Day 15
;;
;; P1
;;

(defun total-risk (path)
  (reduce #'+ path))
