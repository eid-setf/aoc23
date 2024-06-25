(ql:quickload :str)
(ql:quickload :cl-ppcre)
(ql:quickload :alexandria)

(use-package :alexandria)

(load "utils.lisp")

(defun get-game-id (line)
  (let* ((f (position-if #'digit-char-p line))
         (l (position-if-not #'digit-char-p line :start f)))
    (parse-integer (subseq line f l))))

(defun get-plays (line)
  (cdr (str:split ", " (ppcre:regex-replace-all "(;|:)" line ","))))

(defun possiblep (line)
  (mapcar #'(lambda (play)
              (switch ((char play (1+ (position-if-not #'digit-char-p play))) :test #'char=)
                (#\r (if (> (parse-integer play :junk-allowed t) 12)
                         (return-from possiblep nil)))
                (#\g (if (> (parse-integer play :junk-allowed t) 13)
                         (return-from possiblep nil)))
                (#\b (if (> (parse-integer play :junk-allowed t) 14)
                         (return-from possiblep nil)))))
          (get-plays line)))

#+(or)
(apply #'+
       (mapcar #'(lambda (line)
                   (let ((id (get-game-id line)))
                     (if (possiblep line)
                         id
                         0)))
               (uiop:read-file-lines #p"day2.txt")))

;; another solution
(defun most (color)
  (case color
    (red 12)
    (green 13)
    (blue 14)))

(defun possiblep (play)
  (destructuring-bind (num color) (mapcar #'reread play)
    (<= num (most color))))

#+(or)
(apply #'+
       (mapcar #'(lambda (line)
                   (declare (optimize debug))
                   (let* ((toks (tokens #'alphanumericp line))
                          (id  (parse-integer (cadr toks)))
                          (plays (group (cddr toks) 2)))
                     (dolist (p plays id)
                       (unless (possiblep p)
                         (return 0)))))
               (uiop:read-file-lines #p"day2.txt")))

;; part two
(defun least-possible (plays)
  (let ((r 0) (g 0) (b 0))
    (dolist (p plays)
      (destructuring-bind (n color) (mapcar #'reread p)
        (case color
          (red (if (> n r) (setf r n)))
          (green (if (> n g) (setf g n)))
          (blue (if (> n b) (setf b n))))))
    (values r g b)))

#+(or)
(apply #'+
       (mapcar #'(lambda (line)
                   (declare (optimize debug))
                   (let* ((toks (tokens #'alphanumericp line))
                          (id  (parse-integer (cadr toks)))
                          (plays (group (cddr toks) 2)))
                     (multiple-value-call #'* (least-possible plays))))
               (uiop:read-file-lines #p"day2.txt")))
