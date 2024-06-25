(ql:quickload :cl-ppcre)
(ql:quickload :str)
(ql:quickload :arrow-macros)

(use-package :arrow-macros)

(load "utils.lisp")

(add-package-local-nickname :re :cl-ppcre)

;; we trust the input to contain numbers
(defun get-digits (line)
  (let ((acc (make-string 2 :initial-element #\N)))
    (dostr (c line (parse-integer acc))
           (when (digit-char-p c)
             (if (char= (char acc 0) #\N)
                 (allf c (char acc 0) (char acc 1))
                 (setf (char acc 1) c))))))

#+(or)
(apply #'+ (mapcar #'get-digits
                   (uiop:read-file-lines #p"day1.txt")))

;; better solution
#+(or)
(apply #'+ (mapcar #'(lambda (line)
                       (reread (find-if #'digit-char-p line)
                               (find-if #'digit-char-p line :from-end t)))
                   (uiop:read-file-lines #p"day1.txt")))

;; part two
(defun word-to-digit (str)
  (if (= (length str) 1)
      (digit-char-p (char str 0))
      (str:string-case str
        ("one"   1)
        ("two"   2)
        ("three" 3)
        ("four"  4)
        ("five"  5)
        ("six"   6)
        ("seven" 7)
        ("eight" 8)
        ("nine"  9))))

#+(or)
(apply #'+
       (mapcar #'(lambda (line)
                   (reread (->> line
                             (re:scan-to-strings "([0-9]|one|two|three|four|five|six|seven|eight|nine)")
                             #'word-to-digit)
                           (->> (reverse line)
                             (re:scan-to-strings "([0-9]|eno|owt|eerht|ruof|evif|xis|neves|thgie|enin)")
                             #'reverse
                             #'word-to-digit)))
               (uiop:read-file-lines #p"day1.txt")))
