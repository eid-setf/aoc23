(ql:quickload :anaphora)
(ql:quickload :alexandria)


(use-package :anaphora)
(use-package :alexandria)

(load "utils.lisp")

(defun parse (file)
  (let* ((lines (uiop:read-file-lines file))
         (height (length lines))
         (width (length (car lines)))
         (arr (make-array (list height width) :initial-element #\N :element-type 'character)))
    (loop for i from 0 below height
          and line in lines
          finally (return arr)
          do (loop for j from 0 below width
                   do (setf (aref arr i j) (char line j))))))


(defun positions (test str &optional (start 0))
  (let ((p1 (position-if test str :start start)))
    (if p1
        (let ((p2 (position-if (complement test)
                               str :start p1)))
          (cons (list p1 (if (null p2) (length str) p2))
                (if p2
                    (positions test str p2)))))))

(defun puncp (c)
  (and (graphic-char-p c)
       (not (digit-char-p c))
       (not (char= c #\.))))

(defun surrounds-seq (test arr pos)
  (destructuring-bind (row start end) pos
    (loop for j from start below end
          if (surrounds test arr row j)
            do (return-from surrounds-seq
                 (parse-integer (subseq (row-ref arr row) start end))))
    0))

;; part one
(reduce #'+
        (loop with arr = (parse #p"day3.txt")
              for i from 0 below (array-dimension arr 0)
              nconcing (mapcar #'(lambda (pos)
                                   (surrounds-seq #'puncp arr (cons i pos)))
                               (positions #'digit-char-p (row-ref arr i)))))

;; part two
(defun gearp (c)
  (char= c #\*))

(defun span-of (test arr row col)
  (let* ((seq (row-ref arr row)))
    (list row
          (aif (position-if-not test (subseq seq 0 col) :from-end t)
               (1+ it)
               0)
          (position-if-not test seq :start col))))

(defun surrounding-spans (test arr i j)
  (remove-duplicates (mapcar #'(lambda (pos)
                                 (destructuring-bind (i j) pos
                                   (span-of test arr i j)))
                             (surrounds test arr i j))
                     :test #'equal))

(defun parse-span (arr span)
  (destructuring-bind (row start end) span
    (subseq (row-ref arr row) start end)))

(loop with arr = (parse #p"day3.txt")
      for i from 0 below (array-dimension arr 0)
      sum (loop for j from 0 below (array-dimension arr 1)
                for c = (aref arr i j)
                for spans = (surrounding-spans #'digit-char-p arr i j)
                if (and (gearp c) (= 2 (length spans)))
                sum (reduce #'* (mapcar (compose #'parse-integer (curry #'parse-span arr))
                                        spans))))
