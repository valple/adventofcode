(defpackage :aoe2023
  (:use :cl :arrow-macros)
  (:import-from :alexandria #:iota))

(in-package :aoe2023)

(ql:quickload :cl-ppcre
	      :arrow-macros)

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
