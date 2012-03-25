;;; ex02
(cons 'a (cons 'b (cons 'c nil)))
(cons 'a (cons 'b '(c)))
(cons 'a '(b c))

;;; ex07
(defun has-list? (lst)
  (if (null lst)
      nil
      (or (listp (car lst))
	  (has-list? (cdr lst)))))

;;; ex08
;;; (a)
(defun ndots-recur (n)
  (if (<= n 0)
      nil
      (progn 
	(format t ".")
	(ndots-recur (- n 1)))))

(defun ndots-iter (n)
  (dotimes (i n)
    (format t ".")))

;;; (b)
(defun num-of-a-recur (lst)
  (if (null lst)
      0
      (let ((rest (num-of-a-recur (cdr lst))))
	(if (eq (car lst) 'a)
	    (+ rest 1)
	    rest))))

(defun num-of-a-iter (lst)
  (let ((n 0))
    (dolist (i lst)
      (if (eq i 'a)
	  (incf n)))
    n))

;;; ex09
;;; (a)
(defun summit-a (lst)
  (setf lst (remove nil lst))
  (apply #'+ lst))

;;; (b)
(defun summit-b (lst)
  (if (null lst)
      0
      (let ((x (car lst)))
	(if (null x)
	    (summit-b (cdr lst))
	    (+ x (summit-b (cdr lst)))))))