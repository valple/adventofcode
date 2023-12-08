(defpackage :aoc2023
  (:use :cl :arrow-macros :str)
  (:import-from :alexandria #:iota))

(in-package :aoc2023)

;; Common functions, reusable across days 
(defun read-input-file (filepath)
  (uiop:read-file-lines filepath))

;; D1
(defun d1-line-value (line)
  (->> line
    (d1-first-last-match "[0-9]")
    (reduce #'str:concat)
    parse-integer))

(defun d1-first-last-match (regex str)
  (mapcar #'(lambda (s) (ppcre:scan-to-strings regex s))
	  (list str (reverse str))))


(defun d1-1-solution (filepath)
  (->> filepath
    read-input-file
    (mapcar #'d1-line-value)
    (reduce #'+)))

(defun print-solution (filepath fun)
  (format t "The solution is: ~a" (funcall fun filepath)))

(defun print-d1-1-solution ()
  (print-solution "d1.txt" #'d1-1-solution))

(defun d1-cond-reverter (revp seq)
  (if revp
      (reverse seq)
      seq))

(defun d1-2-line-transform (reversep line)
  (let* ((num-strs (mapcar #'(lambda (x) (d1-cond-reverter reversep (format nil "~r" x)))
			   (iota 9 :start 1)))
	 (regex (reduce #'(lambda (x y) (str:concat x "|" y)) num-strs :initial-value "[0-9]")))
    (d1-cond-reverter reversep
		      (ppcre:regex-replace regex
					   (d1-cond-reverter reversep line)
					   #'(lambda (s a b c d e f)
					       (let ((pos (position (subseq s c d) num-strs :test #'equal)))
						 (if pos
						     (write-to-string (1+ pos))
						     (subseq s c d))))))))

;; This is correcter, doesn't fail on "twone" for example
(defun d1-line-value-correct (line)
  (let* ((num-strs (mapcar #'(lambda (x)(format nil "~r" x))
			   (iota 9 :start 1)))

	 (regex (reduce #'(lambda (x y) (str:concat x "|" y)) num-strs :initial-value "[0-9]"))
	 (num-strs-rev (mapcar #'reverse num-strs))
	 (regex-rev (reduce #'(lambda (x y) (str:concat x "|" y)) num-strs-rev :initial-value "[0-9]")))
    (flet ((match (l ns r)
	     (let* ((m (ppcre:scan-to-strings r l))
		    (pos (position m ns :test #'equal)))
	       (if pos (write-to-string (1+ pos)) m))))
      (parse-integer (str:concat (funcall #'match line num-strs regex)
				 (funcall #'match (reverse line) num-strs-rev regex-rev))))))


(defun d1-2-line-value (line)
  (->> line
    (d1-2-line-transform nil)
    (d1-2-line-transform t)
    d1-line-value))

(defun d1-2-solution (filepath)
  (->> filepath
    read-input-file
    (mapcar #'d1-2-line-value)
    (reduce #'+)))

(defun print-d1-2-solution ()
  (print-solution "d1.txt" #'d1-2-solution))

;;D2

(defun between (n min max)
  (and (>= n min)
       (<= n max)))

(defun d2-1-valid-gamep (&optional (b 0) (r 0) (g 0))
  (and (between b 0 14)
       (between r 0 12)
       (between g 0 13)))

(defun extract-number (s)
  (-<> s
    (ppcre:scan-to-strings "[0-9]+" <>)
    (str:trim-left <> :char-bag '(#\0))
    (if <> (parse-integer <>) 0)))

(defun d2-1-game-value (game-nr validp)
  (if validp
      (extract-number game-nr)
      0))

(defun d2-1-line-value (games)
  (d2-1-valid-gamep (d2-colour-value "b" games)
		    (d2-colour-value "r" games)
		    (d2-colour-value "g" games)))

(defun d2-1-eval-games (game-nr games)
  (d2-1-game-value game-nr
		   (d2-1-line-value games)))

(defun d2-parse-game-line (line eval-fn)
  (destructuring-bind (game-nr games)
      (str:split ":" line)
    (funcall eval-fn game-nr games)))

(defun d2-1-parse-game (line)
  (d2-parse-game-line line #'d2-1-eval-games))

(defun d2-1-solution (filepath)
  (->> filepath
    read-input-file
    (mapcar #'d2-1-parse-game)
    (reduce #'+)))

(defun print-d2-1-solution ()
  (print-solution "d2.txt" #'d2-1-solution))

(defun d2-colour-value (colour games)
  (->> 
    (ppcre:all-matches-as-strings (str:concat "[0-9]+ " colour) games)
    (mapcar #'extract-number)
    (reduce #'max)))

(defun d2-2-eval-games (game-nr games)
  (->> (list "b" "g" "r")
    (mapcar #'(lambda (c) (d2-colour-value c games)))
    (reduce #'*)))

(defun d2-2-parse-game (line)
  (d2-parse-game-line line #'d2-2-eval-games))

(defun d2-2-solution (filepath)
  (->> filepath
    read-input-file
    (mapcar #'d2-2-parse-game)
    (reduce #'+)))

(defun print-d2-2-solution ()
  (print-solution "d2.txt" #'d2-2-solution))

;; D3
(defun d3-coordinates-square (xmin xmax ymin ymax)
  (loop for i from xmin to xmax
	with z = '() do
	  (loop for j from ymin to ymax do
	    (push (list i j) z))
	finally (return z)))

(defun d3-1-remove-before-x (x l)
  (remove-if #'(lambda (c) (< (car c) x)) l))

(defun adjoin-all (items list)
  (reduce #'(lambda (x y) (adjoin y x :test #'equal)) items :initial-value list))

(defun d3-1-parse-lines (lines)
  (let ((nums '())
	(matched '())
	(sum 0))
    (loop for i from 0 below (list-length lines) do
      (let ((line (nth i lines)))
	(setf matched (d3-1-remove-before-x (- i 2) matched))
	(ppcre:do-matches (start end "[0-9]+|[^.]" line)
	  (if (str:digitp (subseq line start (1+ start)))
	      (let ((nr (parse-integer (subseq line start end))))
		(progn (setf nums (push (list (d3-coordinates-square i i start (1- end)) nr) nums))
		       (incf sum nr)))
	      (setf matched (adjoin-all (d3-coordinates-square (1- i) (1+ i) (1- start) (1+ start)) matched))))
	(setf nums (remove-if #'(lambda (c) (intersection (car c) matched :test #'equal)) nums)))
	  finally (return (reduce #'- (mapcar #'second nums) :initial-value sum)))))

(defun d3-1-solution (filepath)
   (->> filepath
     read-input-file
     d3-1-parse-lines))

(defun d3-2-remove-before-x (x list)
  (remove-if #'(lambda (c)
		 (< (caaar c) x)) list))

(defun d3-2-parse-lines (lines)
  (let ((nums '())
	(sum 0)
	(stars '()))
    (loop for i from 0 below (list-length lines) do
      (let ((line (nth i lines)))
	(ppcre:do-matches (start end "[0-9]+|[^.]" line)
	  (if (str:digitp (subseq line start (1+ start)))
	      (let ((nr (parse-integer (subseq line start end))))
		(setf nums (push (list (d3-coordinates-square i i start (1- end)) nr) nums)))
	      (if (equal "*" (subseq line start (1+ start)))
		  (setf stars (append stars (list (list i start)))))))
	(loop for star in stars do
	  (if (= (1- i) (car star))
	      (let* ((nbh (d3-coordinates-square (- i 2) i (1- (cadr star)) (1+ (cadr star))))
		    (adj-gears (remove-if-not #'(lambda (c)
						  (intersection (car c) nbh :test #'equal)) nums)))
		(if  (= 2 (list-length adj-gears))
		     (incf sum (reduce #'* (mapcar #'cadr adj-gears)))))))
	(setf nums (d3-2-remove-before-x (1- i) nums))
	(setf stars (d3-1-remove-before-x i stars))
	(if (= i (list-length lines))
	    (loop for star in stars do
	      (if (= i (car star))
		  (let* ((nbh (d3-coordinates-square (- i 1) i (1- (cadr star)) (1+ (cadr star))))
			 (adj-gears (remove-if-not #'(lambda (c)
						       (intersection (car c) nbh :test #'equal)) nums)))
		    (if  (= 2 (list-length adj-gears))
			 (incf sum (reduce #'* (mapcar #'cadr adj-gears)))))))))
	  finally (return sum))))

(defun d3-2-solution (filepath)
  (->> filepath
    read-input-file
    d3-2-parse-lines))

;; D4

(defun d4-1-str-to-nr-list (str)
  (->> str
    (ppcre:all-matches-as-strings "[0-9]+")
    (mapcar #'parse-integer)))

(defun d4-1-parse-line (line)
  (->> line
    (str:split ":")
    second
    (str:split "|")
    (mapcar #'d4-1-str-to-nr-list)))

(defun d4-1-eval-line (line-nrs)
  (->> line-nrs
    d4-line-matches
    1-
    (expt 2)
    floor))

(defun d4-line-matches (line-nrs)
  (->> (intersection (first line-nrs)
		     (second line-nrs))
    list-length))

(defun d4-2-eval-lines (lines)
  (let* ((n (list-length lines))
	 (scratchcards (iota n :start 0 :step 0)))
    (loop for i from 0 below n do
      (let ((match-nr (d4-line-matches (nth i lines)))
	    (current-val (1+ (nth i scratchcards))))
	(progn
	  (setf (nth i scratchcards) current-val)
	  (loop for j from 1 to match-nr do
	    (when (< (+ i j) n)
	      (incf (nth (+ i j) scratchcards) current-val)))))
      finally (return (reduce #'+ scratchcards)))))

(defun d4-1-solution (filepath)
  (->> filepath
    read-input-file
    (mapcar #'d4-1-parse-line)
    (mapcar #'d4-1-eval-line)
    (reduce #'+)))

(defun d4-2-solution (filepath)
  (->> filepath
    read-input-file
    (mapcar #'d4-1-parse-line)
    d4-2-eval-lines))

;; D5
(defun d5-parse-all (seeds lines)
  (let ((curr-vals seeds)
	(new-vals '()))
    (loop for line in lines do
      (if (equal line "")
	  (progn (setf curr-vals (append curr-vals new-vals))
		 (setf new-vals '()))
	  (unless (ppcre:scan "[a-zA-Z]" (subseq line 0 1))
	    (let* ((nums (d4-1-str-to-nr-list line))
		   (target (car nums))
		   (source (cadr nums))
		   (sourceend (+ source (caddr nums))))
	      (loop for val in curr-vals do
		(let ((a (first val))
		      (b (second val)))
		  (when (interval-intersectp a b source sourceend)
		    (let ((y1 (max a source))
			  (y2 (min b sourceend)))
		      (setf curr-vals (remove val curr-vals :test #'equal))
		      (when (< a source)
			(setf curr-vals (append curr-vals (list (list a (1- source))))))
		      (when (> b sourceend)
		      (setf curr-vals (append curr-vals (list (list (1+ sourceend) b)))))
		      (setf new-vals (append new-vals (list (list (+ target (- y1 source))
								  (+ target (- y2 source)))))))))))))
	  finally (return
		    (reduce #'min
			    (mapcar #'car (append curr-vals new-vals)))))))

(defun interval-intersectp (start-a end-a start-b end-b)
  (not (or (< end-b start-a)
	   (< end-a start-b))))

(defun d5-1-solution (filepath)
  (let* ((lines (read-input-file filepath))
	 (seeds (mapcar #'(lambda (x) (list x x))
			(d4-1-str-to-nr-list (car lines)))))
    (d5-parse-all seeds (cdr (cdr lines)))))

(defun d5-2-solution (filepath)
  (let* ((lines (read-input-file filepath))
	 (seeds  (loop for (a b) on (d4-1-str-to-nr-list (car lines))
			 by #'cddr while b
		       collect (list a (+ a b)))))
    (d5-parse-all seeds (cdr (cdr lines)))))

;; D6
(defun d6-value (a b)
  "Solve quadratic equation (a - x) * x = (b + 1), and count integer numbers between it"
  (let* ((d (sqrt (- (/ (expt a 2) 4)
		     (1+ (coerce b 'double-float)))))
	 (x (ceiling (- (/ a 2) d)))
	 (y (floor (+ (/ a 2) d))))
    (1+ (- y x))))

(defun d6-1-solution (filepath)
  (let* ((lines (read-input-file filepath))
	 (a (d4-1-str-to-nr-list (first lines)))
	 (b (d4-1-str-to-nr-list (second lines))))
    (reduce #'* (mapcar #'d6-value a b))))

(defun d6-concat-nr-str (str)
  (->> str
    (str:replace-all " " "")
    (ppcre:scan-to-strings "[0-9]+")
    parse-integer))

(defun d6-2-solution (filepath)
  (let* ((lines (read-input-file filepath))
	 (a (d6-concat-nr-str (first lines)))
	 (b (d6-concat-nr-str (second lines))))
    (d6-value a b)))

;; D7
(defun d7-input-parser (jokerp lines)
  (mapcar #'(lambda (x) (let ((y (str:split " " x)))
			  (list (list (first y)
				      (if jokerp
					  (d7-2-hand-type (first y))
					  (d7-hand-type (first y))))
				(parse-integer (second y)))))
	  lines))

(defun d7-hand-type (hand)
  (let ((counts (make-hash-table :test #'equal)))
    (loop for c across hand do
      (if (gethash c counts)
	  (incf (gethash c counts))
	  (setf (gethash c counts) 1))
	  finally (return (case (hash-table-count counts)
			    (1 6)
			    (2 (if (find 1 (alexandria:hash-table-values counts))
				   5
				   4))
			    (3 (if (find 3 (alexandria:hash-table-values counts))
				   3
				   2))
			    (4 1)
			    (5 0))))))

(defun d7-lower-value (jokerp fir-hand-str sec-hand-str)
  (loop for x across fir-hand-str
	for y across sec-hand-str do
	  (let ((val (d7-issmallercard jokerp x y)))
	    (when (not (= 0 val))
	      (return val)))))

(defun d7-issmallercard (jokerp a b)
  (let* ((chars (coerce (if jokerp
			    "AKQT98765432J"
			    "AKQJT98765432") 'list))
	 (x (position a chars))
	 (y (position b chars)))
    (if (>= x y)
	(if (= x y)
	    0
	    1)
	-1)))

(defun d7-smaller-hand-p (jokerp first-hand second-hand)
  (if (= (second first-hand) (second second-hand))
      (if (= 1 (d7-lower-value jokerp
			       (first first-hand)
			       (first second-hand)))
	  t
	  nil)
      (< (second first-hand) (second second-hand))))

(defun d7-smaller-line (jokerp fline sline)
  (d7-smaller-hand-p jokerp (first fline) (first sline)))

(defun d7-sort-input (jokerp lines)
  (sort lines #'(lambda (x y) (d7-smaller-line jokerp x y))))

(defun d7-eval (lines)
  (loop for i from 0 below (list-length lines)
	sum (* (1+ i) (second (nth i lines)))))

(defun d7-1-solution (filepath)
  (->> filepath
    read-input-file
    (d7-input-parser nil)
    (d7-sort-input nil)
    d7-eval))

(defun d7-2-hand-type (hand)
  (if (str:containsp "J" hand)
      (loop for s across hand
	    maximize (if (equal s #\J)
			 -1
			 (d7-hand-type (str:replace-all "J" (string s) hand)))
	      into m
	    finally (return (if (< m 0) 6 m)))
      (d7-hand-type hand)))

(defun d7-2-solution (filepath)
  (->> filepath
    read-input-file
    (d7-input-parser t)
    (d7-sort-input t)
    d7-eval))

;; D8
(defun circular-nth (index seq)
  (let ((n (mod index (length seq))))
    (subseq seq n (1+ n))))

(defun d8-parse-dir-line (str)
  (let ((parts (ppcre:all-matches-as-strings "[A-Z]+" str)))
    (list (first parts)
	  (list (second parts)
		(third parts)))))

(defun d8-line-to-dict (dict elem)
  (progn
    (setf (gethash (first elem) dict) (second elem))
    dict))

(defun d8-parse-input (lines)
  (let ((dir-dict (make-hash-table :test #'equal))
	(instr (car lines)))
    (values
     instr
     (-<> (cdr (cdr lines))
       (mapcar #'d8-parse-dir-line <>)
       (reduce #'d8-line-to-dict <> :initial-value dir-dict)))))

(defun d8-apply-path (current dir dict)
  (if (equal dir "L")
      (first (gethash current dict))
      (second (gethash current dict))))

(defun d8-1-solution (filepath)
  (multiple-value-bind (instr dict)
      (->> filepath
	read-input-file
	d8-parse-input)
    (let ((current "AAA")
	  (i 0))
      (loop while (not (equal current "ZZZ")) do
	(progn
	  (setf current (d8-apply-path current
				       (circular-nth i instr)
				       dict))
	  (incf i))
	finally (return i)))))

(defun d8-steps-first-z (start instr dict)
  (let ((cur start)
	(i 0))
    (loop while (not (and (> i 0) (equal (str:s-last cur) "A"))) do
      (progn
	(setf cur (d8-apply-path cur
				 (circular-nth i instr)
				 dict))
	(incf i))
	  finally (return i))))

;; Disappointing way to solve it honestly
;; Yeah it works coz
;; time-until-first-z = time-from-first-to-second-z (with same z) = multiple of instruction length
;; Still that's very unsatisfactory
(defun d8-2-solution-2 (filepath)
  (multiple-value-bind (instr dict)
      (->> filepath
	read-input-file
	d8-parse-input)
    (let* ((astr (remove-if-not #'(lambda (x) (equal (str:s-last x) "A"))
				(alexandria:hash-table-keys dict)))
	   (len (length astr))
	   (current (make-array len
				:element-type 'string
				:initial-contents astr)))
      (loop for cur across current
	    collect (d8-steps-first-z cur instr dict) into reses
	    finally (return (apply #'lcm reses))))))

;;only used for some testing
(defun d8-steps-until (start endfn instr dict)
  (let ((cur start)
	(i 0)
	(steps (list start)))
    (loop while (not (and (> i 0) (funcall endfn cur))) do
      (progn
	(setf cur (d8-apply-path cur
				 (circular-nth i instr)
				 dict))
	(setf steps (append steps (list cur)))
	(incf i)
	(when (> i 1000000)
	  (return nil)))
	  finally (return (list i steps)))))

