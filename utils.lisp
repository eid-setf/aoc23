(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
                (let ((rest (nthcdr n source)))
                  (if (consp rest)
                      (rec rest (cons (subseq source 0 n) acc))
                    (nreverse (cons source acc))))))
          (if source (rec source nil) nil)))


(defun mkstr (&rest args)
  (with-output-to-string (s)
                         (dolist (a args) (princ a s))))

(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))))


(defun tokens (test str &optional (start 0))
  (let ((p1 (position-if test str :start start)))
    (if p1
        (let ((p2 (position-if (complement test)
                               str :start p1)))
          (cons (subseq str p1 p2)
                (if p2
                    (tokens test str p2)))))))

(defmacro allf (val &rest args)
  (with-gensyms (gval)
    `(let ((,gval ,val))
       (setf ,@(mapcan #'(lambda (a) (list a gval))
                       args)))))

(defmacro dostr ((var str &optional result) &body body)
  (let ((gi (gensym))
        (glen (gensym)))
    `(do ((,glen (length ,str))
          (,gi 0 (1+ ,gi)))
         ((>= ,gi ,glen) ,result)
       (let ((,var (char ,str ,gi)))
         ,@body))))

(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))


;; custom
(defmacro nloop (binds &body body)
  (labels ((rec (b n)
             (if (null b)
                 `(progn ,@body)
                 (let ((rest (rec (cdr b) (1+ n))))
                   `(loop for ,(caar b) from ,(cadar b) to ,(caddar b)
                          do ,rest)))))
    (rec binds 0)))

(nloop ((i -1 1)
        (j -1 1))
  (format t "~A ~A~%" i j))

(defmacro surrounds (fn arr &rest coords)
  (let* ((garr (gensym))
         (gblock (gensym))
         (gvalue (gensym))
         (gacc (gensym))
         (dims (length coords))
         (syms (loop repeat dims collect (gensym)))
         (gcoords (loop repeat dims collect (gensym)))
         (gargs (mapcar #'(lambda (gc s) `(+ ,gc ,s))
                        gcoords syms)))
    `(let ((,gacc nil)
           (,garr ,arr)
           ,@(mapcar (lambda (g c)
                       `(,g ,c))
                     gcoords
                     coords))
       (nloop ,(mapcar #'(lambda (sym)
                           `(,sym -1 1))
                       syms)
         (unless (= 0 ,@syms)
           (and ,@(loop for gc in gcoords
                        and s in syms
                        and n from 0 below dims
                        collect `(<= 0 (+ ,gc ,s) (1- (array-dimension ,garr ,n))))
                (funcall ,fn (aref ,garr ,@gargs))
                (push (list ,@gargs) ,gacc))))
       (nreverse ,gacc))))


#+(or)
(surrounds #'digit-char-p (make-array '(2 2) :initial-contents (list (list #\1 #\l)
                                                                     (list #\l #\l)))
           1 1)

#+(or)
(surrounds #'digit-char-p (make-array '(2 2 2) :initial-contents (list (list (list #\h #\e)
                                                                             (list #\l #\l))
                                                                       (list (list #\1 #\y)
                                                                             (list #\o #\u))))
           1 1 1)


#+(or)
(surrounds #'digit-char-p "he4llo world"
           3)


;; Sadly this elegant solution doesn't work
;; (defun surrounds (test arr &rest coords)
;;   (declare (optimize debug))
;;   (labels ((rec (arr coords selfp)
;;              (cond
;;                ((null coords)
;;                 (unless selfp
;;                   (funcall test arr)))
;;                (t (or (rec (aref arr (bound (1- (car coords))
;;                                             0 (array-dimension arr 0)))
;;                            (cdr coords)
;;                            nil)
;;                       (rec (aref arr (car coords))
;;                            (cdr coords)
;;                            t)
;;                       (rec (aref arr (bound (1+ (car coords))
;;                                             0 (array-dimension arr 0)))
;;                            (cdr coords)
;;                            nil))))))
;;     (rec arr coords nil)))

(defun ensure-length (lst n &optional (fill nil))
  (labels ((rec (lst n acc)
             (cond
               ((= n 0) (nreverse acc))
               ((null lst)
                (rec lst (1- n)
                     (cons fill acc)))
               (t (rec (cdr lst) (1- n)
                       (cons (car lst) acc))))))
    (rec lst n nil)))

(defun row-ref (arr &rest subscripts)
  (let ((len (length subscripts))
        (rank (array-rank arr)))
    (make-array (if (>= len rank)
                    1
                    (array-dimension arr len))
                :element-type (array-element-type arr)
                :displaced-to arr
                :displaced-index-offset (apply #'array-row-major-index arr
                                               (ensure-length subscripts rank 0)))))

#+(or)
(row-ref (make-array '(2 2) :initial-contents (list (list #\1 #\l)
                                                    (list #\l #\l)))
         1)

#+(or)
(row-ref (make-array '(2 2 2) :initial-contents (list (list (list #\h #\e)
                                                            (list #\l #\l))
                                                      (list (list #\1 #\y)
                                                            (list #\o #\u))))
         1 1 0)


#+(or)
(row-ref "he4llo world" 3)
