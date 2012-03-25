;;; ex01
;;; (a)
((lambda (x)
   (cons x x)) (car y))

;;; (b)
((lambda (w)
   (cons w (+ w z))) (car x))

;;; ex02 
(defun mystery (x y)
  (cond ((null y) nil)
	((eql (car y) x) 0)
	(t (let ((z (mystery x (cdr y))))
	     (and z (+ z 1))))))

;;; ex03
(defun cache-square (x)
  (case x
    (1 1)
    (2 4)
    (3 9)
    (4 16)
    (5 25)
    (t (* x x))))

;;; ex05
(defun precedes-iter (x vec)
  (let ((acc nil))
    (dotimes (i (- (length vec) 1))
      (when (eql (elt vec (+ i 1)) x)
	  (push (elt vec i) acc)))
    (nreverse acc)))

(defun precedes-recur (x vec)
  (let ((acc nil))
    (labels ((ph (x vec end)
	       (let ((i (position x vec
				  :from-end t
				  :end end)))
		 (if (and i (> i 0))
		     (progn
		       (push (elt vec (- i 1)) acc)
		       (ph x vec i))
		     acc))))
      (ph x vec (length vec)))))

;;; ex06
(defun intersperse-iter (x lst)
  (let ((result nil))
    (when (not (null lst))
      (push (car lst) result)
      (dolist (elem (cdr lst) (nreverse result))
	(push x result)
	(push elem result)))))

(defun intersperse-recur (x lst)
  (when (not (null lst))
    (cons (car lst)
	  (when (not (null (cdr lst)))
	    (cons x (intersperse-recur x (cdr lst)))))))

;;; ex07
(defun square (x)
  (* x x))
;;; (a)
(defun differ1?-recur (lst)
  (if (or (null lst) (null (cdr lst)))
      nil
      (labels ((dh (x a-lst)
		 (if (and
		      (not (null a-lst))
		      (= (square (- x (car a-lst))) 1))
		     (dh (car a-lst) (cdr a-lst))
		     (null a-lst))))
	(dh (car lst) (cdr lst)))))

;;; (b)
(defun differ1?-do (lst)
  (and (not (null lst))
       (not (null (cdr lst)))
       (do ((l lst (cdr l)))
	   ((or (null (cdr l))
		(not (= (square (- (first l) (second l))) 1)))
	    (null (cdr l))))))

;;; (c)
(defun differ1?-mapc (lst)
  (and (not (null lst))
       (not (null (cdr lst)))
       (not (not
	     (mapc #'(lambda (x y)
		       (when (not (= (square (- x y)) 1))
			 (return-from differ1?-mapc nil)))
		   lst (cdr lst))))))

;;; ex08
(defun max-min (vec)
  (let ((len (length vec)))
    (if (zerop len)
	(values nil nil)
        (let ((v (svref vec 0)))
	  (case len
	    (1 (values v v))
	    (t
	     (multiple-value-bind (submax submin)
		 (max-min (subseq vec 1))
	       (values (max v submax) (min v submin)))))))))

;;; ex09
(defparameter *net*
  '((a b c)
    (b c)
    (c d)))

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))
;;; (a)
(defun bfs (end queue net)
  (if (null queue)
      nil
      (let ((path (car queue)))
        (let ((node (car path)))
          (if (eql node end)
              (reverse path)
              (catch 'found
		(bfs end
                   (append (cdr queue)
                           (new-paths path node net end))
                   net)))))))

(defun new-paths (path node net end)
  (mapcar #'(lambda (n)
	      (if (eql n end)
		  (progn
		    (format t "Found!")
		    (throw 'found (reverse (cons n path))))
		  (cons n path)))
          (cdr (assoc node net))))

;;; (b)
(defun bfs (end queue net)
  (if (null queue)
      nil
      (let* ((path (car queue))
	     (node (car path)))
        (flet ((new-paths (path node net end)
		 (mapcar #'(lambda (n)
			     (if (eql n end)
				 (progn
				   (format t "Found!")
				   (return-from bfs (reverse (cons n path))))
				 (cons n path)))
			 (cdr (assoc node net)))))
          (if (eql node end)
              (reverse path)
	      (bfs end
                   (append (cdr queue)
                           (new-paths path node net end))
                   net))))))
