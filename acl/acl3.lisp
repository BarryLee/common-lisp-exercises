;;; ex02
(defun new-union (lst1 lst2)
  (if (null lst2)
      lst1
      (let ((a (car lst2)))
	(if (member a lst1)
	    (new-union lst1 (cdr lst2))
	    (new-union (append lst1 `(,a)) (cdr lst2))))))

;;; ex03
(defun occurrences (lst)
  (let ((res nil)
	(occ nil))
    (dolist (n lst)
      (if (setf occ (assoc n res))
	  (setf (cdr occ) (+ (cdr occ) 1))
	  (push (cons n 1) res)))
    (sort res #'> :key #'cdr)))

;;; ex05
;;; (a)
(defun pos+-recur (lst)
  (pos+-recur-iter 0 lst))

(defun pos+-recur-iter (i lst)
  (if (null lst)
      nil
      (cons (+ (car lst) i)
	    (pos+-recur-iter (+ i 1) (cdr lst)))))

;;; (b)
(defun pos+-iter (lst)
  (let ((i 0)
	(res nil))
    (dolist (x lst (reverse res))
      (setf res (cons (+ x i) res))
      (incf i))))

;;; (c)
(defun pos+ (lst)
  (let ((i -1))
    (mapcar #'(lambda (x)
		(incf i)
		(+ x i))
	    lst)))

;;; ex06
;;; (a)
(defun gov-cons (x y)
  (cons y x))

;;; (b)
(defun gov-list (&rest args)
  (if (null args)
      nil
      (if (null (cdr args))
	  (cons nil (car args))
	  (cons (apply #'gov-list (cdr args))
		(car args)))))

;;; (c)
(defun gov-length (glst)
  (if (null glst)
      0
      (+ (gov-length (car glst)) 1)))

;;; (d)
(defun gov-member (item glst)
  (if (null glst)
      nil
      (let ((a (cdr glst)))
	(if (eql a item)
	    glst
	    (gov-member item (car glst))))))

;;; ex07
(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))

(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst)))
	(if (eql elt next)
	    (compr elt (+ n 1) (cdr lst))
	    (cons (n-elts elt n)
		  (compr next 1 (cdr lst)))))))

(defun n-elts (elt n)
  (if (> n 1)
      (cons elt n)
      elt))

;;; ex08
(defun showdots (lst)
  (format t "(")
  (if (null lst)
      (format t ")")
      (progn
	(format t "~a . " (car lst))
	(if (null (cdr lst))
	    (format t "~a)" nil)
	    (showdots (cdr lst))))))

(defun showdots2 (lst)
  (if (atom lst)
      (format t "~a" lst)
      (progn
	(format t "(")
	(showdots2 (car lst))
	(format t " . ")
	(showdots2 (cdr lst))
	(format t ")"))))

;;; ex09
(defparameter *net*
  '((a b c)
    (b c)
    (c d a)
    (d b c)))

(defun longest-path (start end net)
  (let ((longest nil))
    (bfs end (list (list start)) net longest)))

(defun bfs (end queue net longest)
  (if (null queue)
      (reverse longest)
      (let ((path (car queue)))
	(print queue)
	(let ((node (car path)))
	  (if (eql end node)
	      (progn
		(setf longest path)
		(format t "longest: ~a~%" (reverse longest))
		(bfs end (cdr queue) net longest))
	      (bfs end
		   (append (cdr queue)
			   (new-paths path node net))
		   net longest))))))

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
	      (cons n path))
	  (remove-if #'(lambda (n) (member n path))		     
		     (cdr (assoc node net)))))