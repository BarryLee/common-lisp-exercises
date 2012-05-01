;;; ex01
(defun constituent (c)
  (and (graphic-char-p c)
       (not (char= c #\ ))))

(defun tokens (str &key (test #'constituent) (start 0))
  (let ((p1 (position-if test str :start start)))
    (if p1
	(let ((p2 (position-if #'(lambda (c)
				   (not (funcall test c)))
			       str :start p1)))
	  (cons (subseq str p1 p2)
		(if p2
		    (tokens str :test test :start p2)))))))

;;; ex02
(defun bin-search (item seq &key (key #'identity) (test #'eql)
		   (test-less-than #'<) (start 0) (end (- (length seq) 1)))
  (if (> start end)
      nil
      (let* ((mid (floor (/ (+ start end) 2)))
	     (mid-elem (funcall key (elt seq mid))))
	(if (funcall test item mid-elem)
	    mid
	    (progn
	      (if (funcall test-less-than item mid-elem)
		  (setf end (- mid 1))
		  (setf start (+ mid 1)))
	      (bin-search item seq
			  :key key :test test
			  :test-less-than test-less-than
			  :start start :end end))))))

;;; ex03
(defun count-args (&rest args)
  (length args))

;;; ex04
(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let ((biggest (car lst))
	    (bigger (cadr lst)))
	(dolist (elem (cdr lst) (values biggest bigger))
	  (let ((orders (sort `(,biggest ,bigger ,elem) #'> :key fn)))
	    (setf biggest (car orders)
		  bigger (cadr orders)))))))

;;; ex05
(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
	(if val (push val acc))))
    (nreverse acc)))

(defun my-remove-if (predicate lst)
  (filter #'(lambda (x)
	      (if (funcall predicate x)
		  nil
		  x)) lst))

;;; ex06
(let ((gsf nil))
  (defun greatest-seen (x)
    (when (or (null gsf)
	      (> x gsf))
      (setf gsf x))
    gsf))

;;; ex07
(let ((last-arg nil))
  (defun greater? (x)
    (unwind-protect
	 (if (or (null last-arg)
		 (<= x last-arg))
	     (return-from greater? nil)
	     (return-from greater? t))
      (setf last-arg x))))

;;; ex08
(defun expensive (x)
  (if (and (>= x 0) (<= x 100))
      (progn (sleep 3) x)
      (error "x must be between 0 and 100!")))

(let ((cache (make-array 101 :initial-element nil)))
  (defun frugal (x)
    (or (aref cache x)
	(setf (aref cache x) (expensive x)))))

;;; ex09
(defun octal-apply (fn &rest args)
  (let ((*print-base* 8))
    (apply fn args)))