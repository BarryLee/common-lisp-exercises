;;; ex01
(defun quarter-turn (m)
  (let ((n (car (array-dimensions m))))
    (let ((turned-matrix
	   (make-array (list n n) :initial-element nil)))
      (dotimes (r n)
	(dotimes (c n)
	  (setf (aref turned-matrix c (- n r 1))
		(aref m r c))))
      turned-matrix)))

(defun quarter-turn-destructive (m)
  (let ((n (car (array-dimensions m))))
    (flet ((new-pos (x y)
	     (list y (- n x 1))))
      (do ((layer 0 (incf layer))
	   (c 0 (incf c)))
	  ((> layer (/ (- n 1) 2)) m)
	(format t "1st: layer = ~a~%" layer)
	(do ((i layer)
	     (j layer (incf j)))
	    ((= j (- n layer 1)))
	  (format t "2nd: i = ~a, j = ~a~%" i j)
	  (let ((origin (list i j))
		(temp nil))
	    (do ((tmp (aref m i j))
		 (nc (new-pos i j) (setf nc (apply #'new-pos nc))))
		((equal nc origin) (setf (apply #'aref m nc) tmp))
	      (format t "3rd: nc = ~a~%" nc)
	      (setf temp (apply #'aref m nc))
	      (setf (apply #'aref m nc) tmp)
	      (setf tmp temp))))))))

;;; ex02
(defun my-copy-list (lst)
  (reduce #'cons lst :from-end t :initial-value nil))

(defun my-reverse (lst)
  (reduce #'(lambda (x y)
	      (cons y x))
	  lst
	  :initial-value nil))

;;; ex03
(defstruct (ternary-tree)
  d ; data
  l ; left child
  m ; middle child
  r ; right child
  )

(defun load-ternary-tree (lst)
  (if (null lst)
      nil
      (let ((l (cadr lst))
	    (m (caddr lst))
	    (r (cadddr lst)))
	(flet ((load-child (chi)
		 (and (listp chi)
		      (load-ternary-tree chi))))
	  (make-ternary-tree
	   :d (car lst)
	   :l (load-child l)
	   :m (load-child m)
	   :r (load-child r))))))

;;; (a)
(defun copy-ternary-tree2 (tt)
  (if (not (ternary-tree-p tt))
      nil
      (make-ternary-tree
       :d (ternary-tree-d tt)
       :l (copy-ternary-tree2 (ternary-tree-l tt))
       :m (copy-ternary-tree2 (ternary-tree-m tt))
       :r (copy-ternary-tree2 (ternary-tree-r tt)))))

(defun deep-copy? (tt1 tt2)
  (or (and (null tt1) (null tt2))
      (and
       (and (not (eql tt1 tt2))
	    (equal (ternary-tree-d tt1) (ternary-tree-d tt2)))
       (deep-copy? (ternary-tree-l tt1) (ternary-tree-l tt2))
       (deep-copy? (ternary-tree-m tt1) (ternary-tree-m tt2))
       (deep-copy? (ternary-tree-r tt1) (ternary-tree-r tt2)))))

;;; (b)
(defun query-data (obj tt)
  (and (ternary-tree-p tt)
       (if (eql obj (ternary-tree-d tt))
	   tt
	   (or (query-data obj (ternary-tree-l tt))
	       (query-data obj (ternary-tree-m tt))
	       (query-data obj (ternary-tree-r tt))))))

;;; ex04
(defun greatest-to-least (bst)
  (if (null bst)
      nil
      (progn 
	(greatest-to-least (node-r bst))
	(format t "~a " (node-elt bst))
	(greatest-to-least (node-l bst)))))

(defun elements-from-greatest-to-least (bst)
  (when bst
    (append
     (elements-from-greatest-to-least (node-r bst))
     (list (node-elt bst))
     (elements-from-greatest-to-least (node-l bst)))))

;;; ex06
;;; (a)
(defun alist2ht (alst)
  (let ((ht (make-hash-table)))
    (dolist (pair alst ht)
      (let ((k (car pair))
	    (v (cdr pair)))
	(multiple-value-bind (value exist?) (gethash k ht)
	  (or exist?
	      (setf (gethash k ht) v)))))))       

;;; (b)
(defun ht2alist (ht)
  (let ((alst nil))
    (maphash #'(lambda (k v)
		 (push (cons k v) alst))
	     ht)
    alst))