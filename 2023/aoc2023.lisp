(defpackage :aoc2023
  (:use :cl :arrow-macros)
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

