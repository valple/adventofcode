(defpackage :aoc2023
  (:use :cl :arrow-macros :str)
  (:import-from :alexandria #:iota #:curry #:rcurry #:switch)
  (:import-from :iterate #:iter))

(in-package :aoc2023)

;; Common functions, reusable across days 
(defun read-input-file (filepath)
  (uiop:read-file-lines filepath))

(defun transpose-list-of-lists (list)
  (apply #'mapcar #'list list))

(defun print-solution (filepath fun)
  (format t "The solution is: ~a" (funcall fun filepath)))

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

;; D9

(defun d9-parse-input-line (sline)
  (->> sline
    (ppcre:all-matches-as-strings "[-0-9]+")
    (mapcar #'parse-integer)))

(assert (equal '(-4 -1 0 3)
	       (d9-parse-input-line "-4 -1 0 3")))

(defun d9-load-input (filepath)
  (->> filepath
    read-input-file
    (mapcar #'d9-parse-input-line)))

(defun d9-diffs (num-list)
  (loop for i from 1 below (length num-list)
	collect (- (nth i num-list) (nth (1- i) num-list))))

(defun d9-sum-last (list n)
  (+ (car (last list)) n))

(defun equalto (item &key (test #'eql))
  (curry test item))

(defun d9-traverse-line (line)
  (let ((cur line))
    (loop while (notevery (equalto 0) cur)
	  collect (setf cur (d9-diffs cur)) into lines
	  finally (return (append (list line) lines)))))

(defun d9-1-eval-triangle (triangle)
  (reduce #'d9-sum-last triangle :from-end t :initial-value 0))

(defun d9-eval-line (eval-fn line)
  (->> line
    d9-traverse-line
    (funcall eval-fn)))

(defun d9-1-all (input)
  (->> input
    (mapcar (curry #'d9-eval-line #'d9-1-eval-triangle))
    (reduce #'+)))

(defun d9-1-solution (filepath)
  (->> filepath
    d9-load-input
    d9-1-all))

(assert (= (d9-1-solution "d9-test.txt")
	   114))

(defun d9-diff-first (list n)
  (- (car list) n))

(defun d9-2-eval-triangle (triangle)
  (reduce #'d9-diff-first triangle :from-end t :initial-value 0))

(defun d9-2-all (input)
  (->> input
    (mapcar (curry #'d9-eval-line #'d9-2-eval-triangle))
    (reduce #'+)))

(defun d9-2-solution (filepath)
  (->> filepath
    d9-load-input
    d9-2-all))

(assert (= (d9-2-solution "d9-test.txt")
	   2))

;; D10
;; The algorithm is good but the implementation rather terrible.
;; Might clean it up later if I'm motivated.

(defun d10-neighbors (grid-pos grid)
  (d10-keep-valid-grid-points
   (let* ((row (car grid-pos))
	 (col (cdr grid-pos))
	 (s (aref grid row col)))
     (switch (s :test #'eql)
       (#\- (list (cons row (1- col)) (cons row (1+ col))))
       (#\| (list (cons (1- row) col) (cons (1+ row) col)))
       (#\J (list (cons row (1- col)) (cons (1- row) col)))
       (#\F (list (cons (1+ row) col) (cons row (1+ col))))
       (#\L (list (cons (1- row) col) (cons row (1+ col))))
       (#\7 (list (cons row (1- col)) (cons (1+ row) col)))
       (#\S (list (cons row (1- col)) (cons row (1+ col))
		  (cons (1+ row) col) (cons (1- row) col)))
       (otherwise nil)))
   grid))
   
(defun d10-connect (pos-old pos-new grid)
  (let ((nbh (remove-if (equalto pos-old :test #'equal)
			(d10-neighbors pos-new grid))))
    (if (= 1 (length nbh))
	(first nbh)
	nil)))

(defun d10-input-grid (input)
  (make-array (list (length input)
		    (length (first input)))
	      :initial-contents (mapcar (rcurry #'coerce 'list) input)))

(defun d10-s-pos (grid)
  (destructuring-bind (rows cols)
      (array-dimensions grid)
    (loop for row from 0 below rows do
      (loop for col from 0 below cols do
	(when (eql #\S (aref grid row col))
	  (return-from d10-s-pos (cons row col)))))))

(defun d10-out-of-grid (grid-pos grid)
  (destructuring-bind (rows cols)
      (array-dimensions grid)
    (and (between (car grid-pos) 0 (1- rows))
	 (between (cdr grid-pos) 0 (1- cols)))))

(defun d10-keep-valid-grid-points (grid-points grid)
  (remove-if-not (rcurry #'d10-out-of-grid grid) grid-points))

(defun keep-non-nil (list)
  (remove-if-not #'identity list))

(defun d10-s-valid-neighbors (sloc grid)
  (keep-non-nil
   (loop for n in (d10-neighbors sloc grid)
	 collect (if (intersection (d10-neighbors n grid)
				   (list sloc)
				   :test #'equal)
		     n))))

(defun d10-build-loops (grid)
  (let* ((sloc (d10-s-pos grid))
	 (dist-table (make-hash-table :test #'equal))
	 (next-pos (d10-s-valid-neighbors sloc grid))
	 (cur-pos (loop for n in next-pos collect sloc))
	 (counter 1))
    (loop while next-pos do
      (progn 
	(loop for i from 0 below (length next-pos) do
	  (if (nth i next-pos)
	      (progn
		(let ((x (gethash (nth i next-pos) dist-table))
		      (newpos (d10-connect (nth i cur-pos)
					   (nth i next-pos)
					   grid)))
		  (setf (gethash (nth i next-pos) dist-table)
		    (if x (min x counter) counter))
		  (if (eql #\S (aref grid (car newpos) (cdr newpos)))
		      (progn
			(setf (nth i cur-pos) nil)
			(setf (nth i next-pos) nil))
		  (progn  
		    (setf (nth i cur-pos) (nth i next-pos))
		    (setf (nth i next-pos) newpos)))))
	      (progn (setf (nth i cur-pos) nil)
		     (setf (nth i next-pos) nil))))
	(setf cur-pos (keep-non-nil cur-pos))
	(setf next-pos (keep-non-nil next-pos))
	(incf counter))
	  finally (return (progn (setf (gethash sloc dist-table) 0)
				 dist-table)))))

(defun d10-1-max-dist (mainloop)
  (floor (/ (length (hash-table-count mainloop)) 2)))

(defun d10-sort-loop (mainloop)
  (sort (alexandria:hash-table-keys mainloop)
	#'(lambda (x y)
	    (if (< (car x) (car y))
		t
		(if (= (car x) (car y))
		    (< (cdr x) (cdr y))
		    nil)))))


(defun d10-in (item list)
  (find item list :test #'equal))

(defun d10-2-nonloop-points (loop-keys)
  (let ((startrow (reduce #'min (mapcar #'car loop-keys)))
	(endrow (reduce #'max (mapcar #'car loop-keys)))
	(startcol (reduce #'min (mapcar #'cdr loop-keys)))
	(endcol (reduce #'max (mapcar #'cdr loop-keys))))
   (set-difference (loop for i from startrow to endrow
			 append
			 (loop for j from startcol to endcol
			       collect (cons i j)))
		   loop-keys
		   :test #'equal)))
  

(defun d10-pos-add (pos rowadd coladd)
  (cons (+ (car pos) rowadd)
	(+ (cdr pos) coladd)))

(defun d10-in-direction (dir pos check-pos)
  (switch (dir :test #'string=)
    ("left" (and (= (car pos) (car check-pos))
		 (< (cdr check-pos) (cdr pos))))
    ("right" (and (= (car pos) (car check-pos))
		  (> (cdr check-pos) (cdr pos))))
    ("up" (and (> (car pos) (car check-pos))
	       (= (cdr check-pos) (cdr pos))))
    ("down" (and (< (car pos) (car check-pos))
		 (= (cdr check-pos) (cdr pos))))))

(defun d10-2-replace-s (grid)
  (let* ((sloc (d10-s-pos grid))
	 (nbh (d10-s-valid-neighbors sloc grid)))
    (setf (aref grid (car sloc) (cdr sloc))
	  (if (d10-in (d10-pos-add sloc 1 0) nbh)
	      (if (d10-in (d10-pos-add sloc 0 1) nbh)
		  #\F
		  (if (d10-in (d10-pos-add sloc 0 -1) nbh)
		      #\7
		      #\|))
	      (if (d10-in (d10-pos-add sloc -1 0) nbh)
		  (if (d10-in (d10-pos-add sloc 0 1) nbh)
		      #\L
		      #\J)
		  #\-)))
    grid))

;; Interior points intersect an uneven number of times
(defun d10-count-intersections (dir grid pointset)
  (multiple-value-bind (set1 set2)
      (switch (dir :test #'string=)
	("left" (values (list #\F #\| #\7)
			(list #\J #\| #\L)))
	("right" (values (list #\F #\| #\7)
			 (list #\J #\| #\L)))
	("up" (values (list #\F #\- #\L)
		      (list #\J #\- #\7)))
	("down" (values (list #\F #\- #\L)
			(list #\J #\- #\7))))
    (min 
     (count-if #'(lambda (x)
		   (d10-in (aref grid (car x) (cdr x)) set1)) pointset)
     (count-if #'(lambda (x)
		   (d10-in (aref grid (car x) (cdr x)) set2)) pointset))))

(defun d10-crosses-count (pos dir loop-points grid)
  (->> loop-points
    (remove-if-not (curry #'d10-in-direction dir pos))
    (d10-count-intersections dir grid)))

;; Multiple directions necessary coz edges
(defun d10-interior-point-p (pos loop-points grid)
  (every #'oddp (list (d10-crosses-count pos "left" loop-points grid)
		      (d10-crosses-count pos "right" loop-points grid)
		      (d10-crosses-count pos "up" loop-points grid)
		      (d10-crosses-count pos "down" loop-points grid))))
  
(defun d10-direct-nbh (point)
  (list (d10-pos-add point 1 0)
	(d10-pos-add point -1 0)
	(d10-pos-add point 0 1)
	(d10-pos-add point 0 -1)))

(defun d10-direct-neighbors (item list)
  (let ((items (list item))
	(connected (list item)))
    (loop while items do
      (progn
	(setf list (set-difference list items :test #'equal))
	(setf items (intersection list
				  (reduce #'append items :key #'d10-direct-nbh)))
	(setf connected (append connected items)))
      finally (return (values connected list)))))

;; It suffices to check a single representant for every connected component
(defun d10-count-interior-points (mainloop grid)
  (let* ((loop-points (alexandria:hash-table-keys mainloop))
	(non-loop-points (d10-2-nonloop-points
			  loop-points))
	(interior-points 0))
    (loop while non-loop-points do
      (let ((point (first non-loop-points)))
	(multiple-value-bind (connected list)
	    (d10-direct-neighbors point non-loop-points)
	  (progn
	    (when (d10-interior-point-p point loop-points grid)
	      (incf interior-points (length connected)))
	    (setf non-loop-points list))))
	  finally (return interior-points))))

(defun d10-2-solution (filepath)
  (let* ((grid (d10-input-grid (read-input-file filepath)))
	 (mainloop (d10-build-loops grid)))
    (d10-count-interior-points mainloop
			       (d10-2-replace-s grid))))

;; D11
(defun d11-read-input (filepath)
  (->> filepath
    read-input-file
    (mapcar (rcurry #'coerce 'list))
    (funcall #'(lambda (x) (make-array (list (length x)
					     (length (first x)))
				       :initial-contents x)))))

(defun d11-expandlocs (grid)
  (destructuring-bind (i j) (array-dimensions grid)
    (values 
     (remove-if
      #'(lambda (x)
	  (loop for c from 0 below j do
	    (when (eql (aref grid x c) #\#)
	      (return t))
		finally (return nil)))
      (iota i))
     (remove-if
      #'(lambda (x)
	  (loop for r from 0 below i do
	    (when (eql (aref grid r x) #\#)
	      (return t))
		finally (return nil)))
      (iota j)))))
	
(defun apply-tuple-pointwise (a b f)
  (cons (funcall f (car a) (car b))
	(funcall f (cdr a) (cdr b))))

(defun add-tuples (a b)
  (apply-tuple-pointwise a b #'+))

(defun sub-tuples (a b)
  (apply-tuple-pointwise a b #'-))

(defun abs-tuple (a)
  (+ (abs (car a)) (abs (cdr a))))

(defun d11-dist (a b)
  (let ((p (sub-tuples b a)))
    (abs-tuple p)))

(defun d11-points (grid)
  (let ((points nil))
    (loop for r from 0 below (array-dimension grid 0) do
      (loop for c from 0 below (array-dimension grid 1) do
	(when (eql #\# (aref grid r c))
	  (setf points (append points (list (cons r c))))))
	  finally (return points))))

(defun d11-distances (points)
  (let ((dists nil)
	(last-point (car points)))
    (loop for p in (cdr points) do
      (progn
	(setf dists (append dists (list (list last-point (list (sub-tuples p last-point))))))
	(setf last-point p))
	  finally (return dists))))

(defun d11-cumulative-extend (distances)
  ;;(if (= 1 (length distances))
   ;;   distances
      (let ((origin (caar distances)))
	(list origin
	      (reduce #'(lambda (list dist)
			  (append list (list (add-tuples (car (last list)) (caadr dist)))))
		      (cdr distances)
		      :initial-value (cadr (car distances))))))

(defun d11-extend-distances (distances)
  (loop for i below (length distances)
	append (list (d11-cumulative-extend (nthcdr i distances)))))

(defun d11-distance-expanded (p q expandrows expandcols expandsize)
  (let* ((minr (min (car p) (car q)))
	 (maxr (max (car p) (car q)))
	 (minc (min (cdr p) (cdr q)))
	 (maxc (max (cdr p) (cdr q)))
	 (row-intersects (length (remove-if-not
				 (rcurry #'between minr maxr)
				 expandrows)))
	(col-intersects (length (remove-if-not
				 (rcurry #'between minc maxc)
				 expandcols)))
	(dist (sub-tuples q p)))
    (cons (+ (car dist) (* (signum (car dist)) row-intersects expandsize))
	  (+ (cdr dist) (* (signum (cdr dist)) col-intersects expandsize)))))

(defun d11-expanded-size (origin dist expandrows expandcols expandsize)
  (d11-distance-expanded origin (add-tuples origin dist) expandrows expandcols expandsize))

(defun d11-sum-distances (origin distances expandrows expandcols expandsize)
  (loop for d in distances
	sum (abs-tuple
	     (d11-expanded-size origin d expandrows expandcols expandsize))))

(defun d11-sum-distances-all (distancelist expandrows expandcols expandsize)
  (loop for distances in distancelist
	sum (d11-sum-distances (car distances) (cadr distances) expandrows expandcols expandsize)))

(defun d11-solution (filepath expandsize)
  (let ((grid (d11-read-input filepath)))
    (multiple-value-bind (rows cols)
	(d11-expandlocs grid)
      (-> grid
	d11-points
	d11-distances
	d11-extend-distances
	(d11-sum-distances-all rows cols expandsize)))))
   
(defun d11-1-solution (filepath)
  (d11-solution filepath 1))

(defun d11-2-solution (filepath)
  (d11-solution filepath 999999))

;; D12

(defun d12-parse-line (raw-line)
  (-<>> raw-line
    (str:split " ")
    (list (car <>)
	  (d4-1-str-to-nr-list (cadr <>)))))

(defun d12-read-input (filepath)
  (->> filepath
    read-input-file
    (mapcar #'d12-parse-line)))

(defun d12-groups-to-regex (groups)
  (->> groups
    (mapcar #'(lambda (x) (str:concat "[?|#]{" (write-to-string x) "}")))
    (reduce #'(lambda (x y)
		(str:concat x "[.|?]+" y)))
    (funcall #'(lambda (y)
		 (str:concat "^[^#]*" y "[^#]*$")))))

(defun d12-surround-str (str s)
  (str:concat s str s))

(defun d12-get-matches (str reg)
  (let ((match-fun (curry
		    #'ppcre:scan reg))
	(valid-strings (list str)))
    (ppcre:do-matches (start end "[?]" str)
      (setf valid-strings
	    (remove-if-not match-fun
			   (alexandria:flatten
			    (loop for s in valid-strings
				  collect (list (substitute #\. #\? s :start start :end end)
						(substitute #\# #\? s :start start :end end)))))))
    valid-strings))

(defun d12-get-matches-groups (str groups)
  (d12-get-matches str
		   (d12-groups-to-regex groups)))

(defun d12-line-matches (line)
  (d12-get-matches-groups (car line)
			  (cadr line)))

(defun d12-count-matches (lines)
  (->> lines
    (mapcar #'d12-line-matches)
    (mapcar #'length)
    (reduce #'+)))

(defun d12-2-firstsecond (lines)
  (loop for line in lines
	collect (list (length (d12-line-matches line))
		      (length (d12-get-matches-groups
			       (str:concat (car line) "?" (car line))
			       (append (cadr line) (cadr line)))))))
;;		      (length (d12-get-matches-groups
;;			       (str:concat (car line) "?" (car line) "?" (car line))
;;			       (append (cadr line) (cadr line) (cadr line)))))))

(defun d12-nth-fold-brute-force (line i)
  (d12-get-matches-groups
   (reduce #'(lambda (x y) (str:concat x "?" y))
	   (loop for i from 1 to i collect (car line)))
   (loop for j from 1 to i
	 append (cadr line))))

;; Doesn't work for many
(defun d12-2-deduce-count (tuples)
  (->> tuples 
    (mapcar #'(lambda (x)
	      (let* ((c1 (car x))
		     (c2 (cadr x))
		     (factor (/ c2 c1)))
		(* c1 (expt factor 4)))))))
   ;; (reduce #'+)))

(defun d12-with-qmark (line)
  (list (length (d12-line-matches line))
	(length (d12-get-matches-groups
		 (str:concat (car line) "?")
		 (cadr line)))
	(length (d12-get-matches-groups
		 (str:concat "?" (car line))
			       (cadr line)))))

(defun arst (s)
  (position #\# s :from-end t))


(defun d12-extraqm (str groups n)
  (d12-get-matches-groups (reduce #'str:concat
				  (loop for i from 1 to n
					collect "?" into k
					finally (return (append k (list str)))))
			  groups))

(defun d12-match-extend (str groups match)
  (let* ((pos (arst match))
	 (len (length str))
	 (lastnon? (car (last (cl-ppcre:all-matches "[^?]" str))))
	 (cutoff (min (max (+ 2 pos) (1+ lastnon?)) len))
	 (matchpart (str:concat (subseq match 0 cutoff)
				(if (and (= cutoff len)
					 (equal (subseq match (1- cutoff) cutoff) "#"))
				    "."
				    "")))
	 (extra? (if (and (= cutoff len)
			  (equal (subseq match (1- cutoff) cutoff) "#"))
		     0
		     (1+ (- len cutoff)))))
    (mapcar (curry #'str:concat matchpart)
	    (d12-extraqm str groups extra?))))

(defun d12-count-trailing-qm (str match)
  (let* ((pos (arst match))
	 (len (length str))
	 (lastnon? (car (last (cl-ppcre:all-matches "[^?]" str))))
	 (cutoff (min (max (+ 2 pos) (1+ lastnon?)) len)))
    (if (and (= cutoff len)
	     (equal (subseq match (1- cutoff) cutoff) "#"))
	0
	(1+ (- len cutoff)))))

(defun d12-count-leading-qm (str match)
  (d12-count-trailing-qm (reverse str) (reverse match)))

(defun d12-sort-matches (str matches)
  (let ((counts (make-hash-table :test #'equal)))
    (loop for match in matches do
      (let* ((leading (d12-count-leading-qm str match))
	    (trailing (d12-count-trailing-qm str match))
	    (count (gethash (cons leading trailing) counts)))
	(setf (gethash (cons leading trailing) counts )
	      (if count
		  (1+ count)
		  1))))
    counts))

(defun d12-test (str groups)
  (let ((matches (d12-get-matches-groups str groups)))
    (alexandria:flatten
     (loop for match in matches
	   collect (list (d12-match-extend str groups match)
			 (mapcar #'reverse (d12-match-extend (reverse str)
							     (reverse groups)
							     (reverse match))))))))

(defun d12-testline (line)
  (d12-test (car line)
	    (cadr line)))




(defun d12-2-line-fancy (line)
  (let* ((str (car line))
	 (last (subseq str (1- (length str)) (length str)))
	 (first (subseq str 0 1))
	 (groups (cadr line)))
    (cond
      ((equal last ".")
       (* (length (d12-get-matches-groups str groups))
	  (expt (length (d12-get-matches-groups
			 (str:concat "?" str) groups))
		4)))
      ((equal first ".")
       (* (length (d12-get-matches-groups str groups))
	  (expt (length (d12-get-matches-groups
			 (str:concat str "?")  groups))
		4)))
      ((and (equal first "#") (equal last "#"))
       (expt (length (d12-get-matches-groups
		      (str:concat str)  groups))
	     5)))
      ))
    
(defun d12-brute-force (line)
  (length 
   (d12-line-matches
    (list (reduce #'(lambda (x y) (str:concat x "?" y))
		  (loop for i from 0 to 4 collect (car line)))
	  (loop for i from 0 to 4 append (cadr line))))))


(defun d12-brute-force-count-all (lines)
   (->> lines
    (mapcar #'d12-brute-force)
    (reduce #'+)))
  
(defun cartesian-str-concat(lista listb)
  (loop for i in lista
	nconc (loop for j in listb
		    collect (str:concat i "?" j))))

(defun d12-2-extendstr (list &optional (times 5))
  (reduce #'cartesian-str-concat (loop for i from 0 below times collect list)))

(defun d12-2-parse-line (line)
  (let ((reg (d12-groups-to-regex
	      (loop for i from 0 to 4 append (cadr line))))
	(matches (d12-line-matches line)))
    (remove-if-not (curry #'ppcre:scan reg)
		   (d12-2-extendstr matches))))

;; D13

(defun d13-str-list-to-grid (lines)
  (let ((rows (length lines))
	(cols (length (car lines))))
    (make-array (list rows cols)
		:initial-contents (mapcar (rcurry #'coerce 'list) lines))))

(defun d13-parse-input (lines)
  (let ((grids nil)
	(lastpos 0))
    (loop for line in lines
	  for i from 0 below (length lines) do
      (when (equal line "")
	(progn
	  (push (d13-str-list-to-grid (subseq lines lastpos i)) grids)
	  (setf lastpos (1+ i)))))
    (push (d13-str-list-to-grid (subseq lines lastpos)) grids)
    grids))

(defun d13-parse-input2 (lines)
  (let ((grids nil)
	(lastpos 0))
    (loop for line in lines
	  for i from 0 below (length lines) do
      (when (equal line "")
	(progn
	  (push (subseq lines lastpos i) grids)
	  (setf lastpos (1+ i)))))
    (push (subseq lines lastpos) grids)
    (mapcar #'(lambda (x)
		(mapcar (rcurry #'coerce 'list) x)) grids)))

(defun d13-h-match-grug (grid n)
  (if (< n (length grid))
      (loop for i from 1 to (min n (- (length grid) n)) do
	(when (not (equal (nth (- n i) grid)
			  (nth (+ (1- n) i) grid)))
	  (return nil))
	    finally (return t))
      nil))

(defun d13-v-match-grug (grid n)
  (d13-h-match-grug (transpose-list-of-lists grid) n))

(defun d13-find-match-grug (grid)
  (loop for i from 1 below (max (length grid) (length (car grid)))
	if (d13-h-match-grug grid i)
	  do (return (* i 100))
	if (d13-v-match-grug grid i)
	  do (return i)))

(defun d13-1-solution-grug (filepath)
  (->> filepath
    read-input-file
    d13-parse-input2
    (mapcar #'d13-find-match-grug)
    (reduce #'+)))


;; D14
(defun d14-parse-input (lines)
  (first (d13-parse-input lines)))

(defun d14-load-grid (filepath)
  (d14-parse-input (read-input-file filepath)))

(defun d14-count-rocks-and-pillars (grid col)
  (loop for row from 0 below (array-dimension grid 0)
	when (eql (aref grid row col) #\O)
	  collect row into rocks
	end
	if (eql (aref grid row col) #\#)
	  collect row into pillars
	end
	finally (return (values (append  (list -1)
				       pillars
				       (list (array-dimension grid 0)))
			      rocks))))

;; Changes the incoming array
(defun d14-tilt-north (grid)
  (destructuring-bind (rows cols)
      (array-dimensions grid)
    (loop for col from 0 below cols do
      (multiple-value-bind (pillars rocks)
	  (d14-count-rocks-and-pillars grid col)
	(loop for plow from 0 below (1- (length pillars))
	      for phigh from 1 to (1- (length pillars)) do
		(let ((firstp (nth plow pillars))
		      (secondp (nth phigh pillars))
		      (count (count-if
			      (rcurry #'between
				      (nth plow pillars)
				      (nth phigh pillars))
			      rocks)))
		  (loop for j from (1+ firstp) below secondp
			for i from 1 below (- secondp firstp)
			if (<= i count)
			  do (setf (aref grid j col) #\O)
			else
			  do (setf (aref grid j col) #\.)
			end))))
	  finally (return grid))))

;; Would be better to change the tilt functions instead of making a new grid with every turn
;; But I'm lazy
(defun d14-cycle (grid n)
  (let ((awesomegrid (alexandria:copy-array grid))
	(cursum 0))
    (loop for i from 1 to n collect 
      (progn (setf awesomegrid (-> awesomegrid
				 d14-tilt-north
				 (aops:turn 1)
				 d14-tilt-north
				 (aops:turn 1)
				 d14-tilt-north
				 (aops:turn 1)
				 d14-tilt-north
				   (aops:turn 1)))
	    ;; (when (> i 100)
	     (let ((val (d14-eval-grid awesomegrid)))
	       (setf cursum (+ cursum val))
	       ;;(format t "Run ~a: ~a, ~$~%" i val (/ cursum (- i 100)) )))))
	       (list i val (/ cursum i)))))))

(defun d14-eval-grid (grid)
  (destructuring-bind (rows cols)
      (array-dimensions grid)
    (loop for j from 0 below cols
	  sum (loop for i from 0 below rows
		    sum (* (- cols i) (if (eql (aref grid i j) #\O)
					  1
					  0))))))

(defun d14-1-solution (filepath)
  (->> filepath
    read-input-file
    d14-parse-input
    d14-tilt-north
    d14-eval-grid))

(defun count-occurences (list select-fn &key (test #'eql))
  (let ((count-dict (make-hash-table :test test)))
    (loop for item in list do
      (let* ((y (funcall select-fn item))
	     (x (gethash y count-dict)))
	(if x
	    (incf (gethash y count-dict))
	    (setf (gethash y count-dict) 1))))
    (loop for key being the hash-key of count-dict
	  collect (list key (gethash key count-dict)))))

(defun d14-spacing (triples)
  (let* ((occurences (count-occurences triples #'second))
	 (candidates (mapcar #'first (remove-if-not #'(lambda (x) (> (second x) 100)) occurences))))
    (loop for c in candidates
	  collect (let ((cseq (remove-if-not #'(lambda (x) (= c (second x))) triples)))
		    (list (subseq (nth (1- (length cseq)) cseq) 0 2)
			  (- (first (nth (1- (length cseq)) cseq))
			     (first (nth (- (length cseq) 2) cseq))))))))

(defun d14-period (spacing)
  (let ((common-spacing
	  (-> spacing
	    (count-occurences #'second :test #'equal)
	    (sort #'(lambda (x y) (> (second x) (second y))))
	    caar)))
    (find-if #'(lambda (x) (= (second x) common-spacing)) spacing)))

;; Could use less cycles but then it doesn't work well for the testset
;; This takes a while but statistically should give the good result
(defun d14-2-deduce-solution (grid)
  (let* ((some-values (d14-cycle grid 10000))
	 (period (d14-period (d14-spacing some-values)))
	 (period-end (caar period))
	 (period-size (second period))
	 (diff (nth-value 1 (ceiling (- 1000000000 period-end)
				     period-size))))
    (second (nth (1- (+ diff period-end)) some-values))))

(defun d14-2-solution (filepath)
  (->> filepath
    d14-load-grid
    d14-2-deduce-solution))

;; Day 15
(defun d15-read-input (filepath)
  (->> filepath
    uiop:read-file-string
    (str:split ",")
    (mapcar #'str:trim)))

(defun d15-strs-to-char-codes (strs)
  (->> strs 
    (mapcar (rcurry #'coerce 'list))
    (mapcar (curry #'mapcar #'char-code))))

(defun d15-parse-chunk (value charcodes)
  (loop for code in charcodes do
    (setf value (rem (* 17 (+ value code)) 256))
	finally (return value)))

(defun d15-1-solution (filepath)
  (-<>> filepath
    d15-read-input
    d15-strs-to-char-codes
    (mapcar (curry #'d15-parse-chunk 0))
    (reduce #'+)))

(defun d15-2-split-chunk (str)
  (let ((labellens (ppcre:split "=|-" str)))
    (list (first labellens)
	  (ppcre:scan-to-strings "=|-" str)
	  (second labellens))))

(defun d15-hashfn (str)
  (->> str
    list
    d15-strs-to-char-codes
    first
    (d15-parse-chunk 0)))

(defun d15-2-strs-to-triples (strs)
  (mapcar #'d15-2-split-chunk strs))

(defun d15-2-parse-all (triples)
  (let ((boxes (make-hash-table)))
    (loop for triple in triples do
      (let* ((label (first triple))
	    (symb (second triple))
	    (lens (nth 2 triple))
	    (hash (d15-hashfn label)))
	(cond ((equal symb "-")
	       (setf (gethash hash boxes)
		     (remove-if #'(lambda (x) (equal (first x) label))
				(gethash hash boxes))))
	      ((equal symb "=")
	       (setf (gethash hash boxes)
		     (let* ((curstack (gethash hash boxes))
			    (pos (position-if #'(lambda (x) (equal (first x) label)) curstack)))
		       (if pos
			   (progn (setf (nth pos curstack) (cons label lens))
				  curstack)
			   (append curstack (list (cons label lens))))))))))
    boxes))


(defun d15-2-eval-dict (boxes)
  (loop for boxnr being the hash-key of boxes
	sum (let ((lenses (mapcar #'cdr (gethash boxnr boxes))))
	      (if lenses
		  (loop for l in lenses
			for n from 1 to (length lenses)
			sum (* n (parse-integer l) (1+ boxnr)))
		  0))))

(defun d15-2-solution (filepath)
  (->> filepath
    d15-read-input
    d15-2-strs-to-triples
    d15-2-parse-all
    d15-2-eval-dict))

;; D16
(defun d16-load-grid (filepath)
  (->> filepath
    read-input-file
    d10-input-grid))

(defun inc-or-1 (n)
  (if n
      (1+ n)
      1))

(defun between-c (cgridsize cnr)
  (and (between (realpart cnr) 0 (1- (realpart cgridsize)))
       (between (imagpart cnr) (1+ (imagpart cgridsize)) 0)))

(defun d16-char-to-new-dir (char dir)
  (alexandria:switch (char :test #'eql)
    (#\\ (- (complex (imagpart dir) (realpart dir))))
    (#\/ (complex (imagpart dir) (realpart dir)))
    (otherwise dir)))

;; a nice and easily debuggable function
(defun d16-energise (start-pos start-dir grid)
  (destructuring-bind (rows cols)
      (array-dimensions grid)
    (let ((positions (list (list start-pos start-dir)))
	  (visited (make-hash-table))
	  (in-grid-p (curry #'between-c (complex cols (- rows)))))
      (setf (gethash start-pos visited) (list start-dir)) ;; for completeness
      (loop while positions do
	(let* ((pos (pop positions))
	       (dir (second pos))
	       (p (first pos))
	       (cur nil)
	       (symb (aref grid (- (imagpart p)) (realpart p))))
	  (cond ((or (eql symb #\/) (eql symb #\\) (eql symb #\.)
		     (and (eql symb #\|) (= 0 (realpart dir)))
		     (and (eql symb #\-) (= 0 (imagpart dir))))
		 (progn (setf dir (d16-char-to-new-dir symb dir))
			(setf cur (+ p dir))
			(when (and (funcall in-grid-p cur) (not (find dir
								      (gethash cur visited))))
			  (progn (push (list cur dir) positions)
				 (setf (gethash cur visited)
				       (push dir (gethash cur visited)))))))
		((eql symb #\-)
		 (let* ((dir1 #C(1 0))
		       (dir2 #C(-1 0))
		       (p1 (+ p dir1))
		       (p2 (+ p dir2)))
		   (when (funcall in-grid-p p1)
		     (when (not (find dir1
				      (gethash p1 visited)
				      :test #'equal))
		       (progn (push (list p1 dir1) positions)
			      (setf (gethash p1 visited)
				    (push dir1 (gethash p1 visited))))))
		   (when (funcall in-grid-p p2)
		     (when (not (find dir2
				      (gethash p2 visited)
				      :test #'equal))
		       (progn (push (list p2 dir2) positions)
			      (setf (gethash p2 visited)
				    (push dir2 (gethash p2 visited))))))))
		((eql symb #\|)
		 (let* ((dir1 #C(0 1))
			(dir2 #C(0 -1))
			(p1 (+ p dir1))
			(p2 (+ p dir2)))
		   (when (funcall in-grid-p p1)
		     (when (not (find dir1
				      (gethash p1 visited)
				      :test #'equal))
		       (progn (push (list p1 dir1) positions)
			    (setf (gethash p1 visited)
				  (push dir1 (gethash p1 visited))))))
		   (when (funcall in-grid-p p2)
		       (when (not (find dir2
					(gethash p2 visited)
					:test #'equal))
			 (progn (push (list p2 dir2) positions)
				(setf (gethash p2 visited)
				      (push dir2 (gethash p2 visited))))))))
		(t (format t "WHATTTT?~%")))))
      visited)))
		
(defun d16-1-solution (filepath)
  (->> filepath
    d16-load-grid
    (d16-energise #C(0 0) #C(1 0))
    hash-table-count))

(defun d16-energised-panels (start-pos start-dir grid)
  (hash-table-count (d16-energise start-pos start-dir grid)))

(defun d16-corner-p (rows cols i j)
  (if (= 0 i)
      (if (or (= j 0) (= j (1- cols)))
	  t
	  nil)
      (if (= (1- rows) i)
	  (if (or (= j 0) (= j (1- cols)))
	      t
	      nil))))

;; I probably could fiddle with the dictionary output to get a fast and elegant solution.
;; Too lazy for that now and it doesn't take that long to run (3 min)
;; Ok that's actually long but I'll think about it another day
(defun d16-test-configs (grid)
  (destructuring-bind (rows cols)
      (array-dimensions grid)
    (let ((reses nil)
	  (energise (rcurry #'d16-energised-panels grid))
	  (dirs nil))
      (loop for i from 0 below rows maximize
	(loop for j from 0 below cols
	      maximize
	      (if (or (= i 0) (= 0 j) (= i (1- rows)) (= j (1- cols)))
		(progn 
		  (when (= 0 i)
		    (setf dirs (append dirs (list #C(0 -1)))))
		  (when (= 0 j)
		    (setf dirs (append dirs (list #C(1 0)))))
		  (when (= (1- rows) i)
		    (setf dirs (append dirs (list #C(0 1)))))
		  (when (= (1- cols) j)
		    (setf dirs (append dirs (list #C(-1 0)))))
		  (loop for dir in dirs maximize (funcall energise (complex j (- i)) dir)))
		0))))))

(defun d16-2-solution (filepath)
   (->> filepath
     d16-load-grid
     d16-test-configs))

