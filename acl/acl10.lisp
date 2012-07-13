;;; ex01
(let ((x 'a)
      (y 'b)
      (z '(c d)))
  ;; (a)
  (print `(,z ,x z))
  ;; (b)
  (print `(x ,y ,@z))
  ;; (c)
  (print `((,@z ,x) z)))

;;; ex02
(defmacro myif (predict then-clause &optional else-clause)
  `(cond (,predict ,then-clause)
	 (t ,else-clause)))

;;; ex03
(defmacro nth-expr (n &rest exprs)
  (nth (- n 1) exprs))

;;; ex04
(defmacro with-gensyms (varlist &body body)
  `(let ,(mapcar #'(lambda (var)
		     `(,var (gensym)))
		 varlist)
     ,@body))

(defmacro ntimes (n &rest body)
  (with-gensyms (f g)
    `(labels ((,f () ,@body)
	      (,g (n)
		(when (> n 1)
		  (,f)
		  (,g (- n 1)))))
       (,g ,n))))

;;; ex05
(defmacro n-of (n expr)
  (with-gensyms (res i)
    `(let ((,res nil))
       (dotimes (,i ,n (nreverse ,res))
	 (push ,expr ,res)))))

;;; ex06
(defmacro safe-eval (varlst &rest body)
  (let ((caches nil)
	(block-name (gensym)))
    (dolist (e varlst)
      (push (gensym) caches))
    `(block ,block-name
       (let (,@(mapcar #'list
		       caches varlst))
	 (unwind-protect 
	      (return-from ,block-name (progn ,@body))
	   ,(append '(setf)
		    (mapcan #'list
			    varlst caches)))))))
       
;;; ex07
;;; seems that the sbcl's implemention of push has multi-eval issue, as
;;; the question's implemention 
(defmacro push-1 (obj lst)
  `(setf ,lst (,obj ,lst)))

;;; the following implemention fixed this issue
(defmacro push-2 (obj lst)
  (let ((a (gensym))
	(b (gensym)))
    `(let ((,a ,obj)
	   (,b ,lst))
       (setf ,b (cons ,a ,b)))))
