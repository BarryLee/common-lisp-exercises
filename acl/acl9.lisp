;;; ex01
(defun non-decending? (rlst)
  (if (null rlst)
      t
      (let ((i (car rlst))
	    (rest (cdr rlst)))
	(cond ((null rest) t)
	      ((> i (car rest)) nil)
	      (t (non-decending? rest))))))

;;; ex02
(defmacro with-gensyms (varlist &body body)
  `(let ,(mapcar #'(lambda (var)
		     `(,var (gensym)))
		 varlist)
     ,@body))

(defmacro mydolist (var-list &body body)
  (let ((var (car var-list)))
    (with-gensyms (lst)
      `(let ((,lst ,(cadr var-list)))
	 (do ((,var (car ,lst)
		    (progn
		      (setf ,lst (cdr ,lst))
		      (car ,lst)))
	      (-i- 0 (incf -i-)))
	     ((null ,lst) ,(caddr var-list))
	   ,@body)))))

(defun mapcat (fn lst &rest more-lsts)
  (let ((res-1 (apply #'mapcar fn lst more-lsts))
	(acc nil))
    (dolist (i res-1)
      (if i (push i acc)))
    (nreverse acc)))

(defmacro doseq (varlist &body body)
  (let ((vns nil)       ; var names
	(lvns nil)      ; var names for lists
	(vvns nil)      ; var names for vectors
	(seqs nil)      ; sequences
	(vars nil)      ; vars
	(var-lsts nil)         ; list type args
	(var-vecs nil))        ; vector type args
    (dolist (var-seq varlist)
      (let ((seq (cadr var-seq))
	    (var (car var-seq))
	    (vn (gensym)))
	(if (listp seq)
	    (progn
	      (push (list var seq) var-lsts)
	      (push vn lvns))
	    (progn
	      (push (list var seq) var-vecs)
	      (push vn vvns)))
	(push seq seqs)
	(push var vars)
	(push vn vns)))
    (with-gensyms (i)
      `(let ,(mapcar
	      #'(lambda (gs seq)
		  `(,gs ,seq))
	      vns seqs)
	 (do ,(append
	       `((,i 0 (incf ,i)))
	       (mapcar
		#'(lambda (gs var)
		    `(,var (elt ,gs 0)))
		vns vars))
	     ((or
	       ,@(mapcar
		  #'(lambda (gs seq)
		      (if (listp seq)
			  `(null ,gs)
			  `(= ,i (length ,gs))))
		  vns seqs)))
	   ,@(mapcar
	      #'(lambda (gs var-vec)
		  `(setf ,(car var-vec) (elt ,gs ,i)))
	      vvns var-vecs)
	   ,@body
	   ,@(mapcar
	      #'(lambda (gs var-lst)
		  `(progn
		     (setf ,gs (cdr ,gs))
		     (setf ,(car var-lst) (car ,gs))))
	      lvns var-lsts))))))

(defun make-changes (amnt)
  (labels ((div (a b)
	     (values (floor (/ a b))
		     (rem a b)))
	   (range (n)
	     (let (res)
	       (dotimes (i n res)
		 (push (decf n) res)))))
    (let* ((denoms '(25 10 5 1))
	   (res (make-array 4 :initial-element 0))
	   (idx (range (length denoms))))
      (doseq ((den denoms)
	      (i idx))
	(multiple-value-bind (d r) (div amnt den)
	  (setf (aref res i) d)
	  (if (= r 0)
	      (return-from make-changes res)
	      (when (/= d 0) (setf amnt r))))))))

;;; ex03
(let ((r (cons 51 49)))
  (defun singing-contest ()
    (let ((res (cons 0 0)))
      (dotimes (i 10)
	(if (< (random (+ (car r) (cdr r)))
	       (car r))
	    (incf (car res))
	    (incf (cdr res))))
      (incf (car r) (car res))
      (incf (cdr r) (cdr res))
      res)))

;;; ex04
(defun line-equation (pa pb)
  (let ((xa (car pa))
	(ya (cdr pa))
	(xb (car pb))
	(yb (cdr pb)))
    (cond ((= xa xb)
	   (list 1 0 xa))
	  ((= ya yb)
	   (list 0 1 ya))
	  (t
	   (let ((k (/ (- yb ya)
		       (- xb xa))))
	     (list k -1 (- (* k xa) ya)))))))

(defun range (x &optional (y nil))
  (if (not y)
      (setf y x
	    x 0))
  (let ((n (- y x)))
    (if (<= n 0)
	(make-array 0)
	(let ((arr (make-array n)))
	  (dotimes (i n)
	    (setf (aref arr i) (+ x i)))
	  arr))))

(defun areverse (arr &key (start 0) (end (- (length arr) 1)))
  (do ((i start (incf i))
       (j end (decf j)))
      ((>= i j) arr)
    (rotatef (aref arr i) (aref arr j))))

(let ((last-perm nil))
  (defun next-perm (&optional p)
    (unless (or p last-perm)
      (return-from next-perm nil))
    (unless p
      (setf p last-perm))
    (let* ((i (- (length p) 2))
	   (j (+ i 1)))
      (do ()
	  ((or (< i 0)
	       (< (aref p i)
		  (aref p (+ i 1)))))
	(decf i))
      (if (< i 0)
	  nil
	  (progn
	    (do ()
		((> (aref p j)
		    (aref p i)))
	      (decf j))
	    (rotatef (aref p j) (aref p i))
	    (setf last-perm (areverse p :start (+ i 1))))))))

(defun number-of-reverses (seq)
  (let ((n 0)
	(len (length seq)))
    (dotimes (i len)
      (let ((a (aref seq i)))
	(do ((j (+ i 1) (incf j)))
	    ((= j len))
	  (when (> a (aref seq j))
	    (incf n)))))
    n))

(defun det (m)
  (labels ((sgn (p)
	     (if (evenp (number-of-reverses p))
		 1
		 -1)))
    (let ((n (car (array-dimensions m)))
	  (res 0))
      (do ((perm (range n) (setf perm (next-perm perm))))
	  ((null perm))
	(let ((prod (sgn perm)))
	  (do ((i 0 (incf i)))
	      ((>= i n))
	    (setf prod (* prod (aref m i (aref perm i)))))
	  (incf res prod)))
      res)))
	      
(defun equations-to-matrix (linear-equations)
  ;; convert to matrix A & vector b for linear equations Ax = b
  (print linear-equations)
  (let* ((m (length linear-equations))
	 (A (make-array (list m m)))
	 (b (make-array m)))
    ;; compose matrix A & vector b
    (let ((i 0))
      (dolist (equation linear-equations)
	(let ((n (length equation)))
	  (unless (= n (+ m 1))
	    (return-from equations-to-matrix nil))
	  (do ((x (car equation))
	       (j 0 (incf j)))
	      ((null (cdr equation)))
	    (setf (aref A i j) x
		  equation (cdr equation))
	    (setf x (car equation)))
	  (setf (elt b i) (car equation))
	  (incf i))))
    (list A b)))

(defun copy-arr (src-arr)
  (let* ((dims (array-dimensions src-arr))
	 (dest-arr (make-array dims)))
    (dotimes (i (car dims))
      (dotimes (j (cadr dims))
	(setf (aref dest-arr i j) (aref src-arr i j))))
    dest-arr))

(defun cramer (A b)
  (print A)
  (print b)
  (let ((da (det A)))
    (print da)
    (unless (= da 0)
      (let* ((n (length b))
	     (res (make-array n)))
	  (dotimes (i n)
	    (let ((m (copy-arr A)))
	      (print m)
	      (dotimes (j n)
		(setf (aref m j i) (aref b j)))
	      (print m)
	      (setf (aref res i) (/ (det m) da))))
	res))))
    
(defun intersect-point (line1-start line1-end line2-start line2-end)
  (apply #'cramer (equations-to-matrix (mapcar #'line-equation
					       (list line1-start line2-start)
					       (list line1-end line2-end)))))

;;; ex05
(defun approx-root (f min max epsilon)
  (let ((i 0)
	(temperature 100000.0)
	(cool 0.99)
	(step (* (min (- min) max) 0.99))
	(scale 0.99)
	(maxstopcnt 3)
	(r (- max min))
	(cnt 0))
    (do ((j (funcall f i) (funcall f i))
	 (stopcnt 0))
	((or (<= (abs j) epsilon)
	     (<= temperature 0.1)))
      (incf cnt)
      (let* ((dir (if (= (random 2) 1) 1 -1))
	     (try (+ i (* dir
			  (random step)))))
;	(cond ((>= try max)
;	       (setf try (- max (* r (- 1 scale)))))
;	      ((<= try min)
;	       (setf try (+ min (* r (- 1 scale))))))
	(if (or (>= try max)
		(<= try min))
	    (setf step (* step scale))
	    (progn
	      (let ((sa (- (abs j)))
		    (sb (- (abs (funcall f try)))))
		(if (or (< sa sb) ; getting better
			(< (random 1.0)
			   (exp (/ (+ sa sb) temperature))))
		    (setf i try)
		    (if (>= (incf stopcnt) maxstopcnt)
			(setf stopcnt 0
			      step (float (* step scale))))))
	      (format t "|~a~a~a| (step: ~a)~%"
		      (make-string
		       (round (- i min)) :initial-element #\ )
		      "*"
		      (make-string
		       (round (- max i)) :initial-element #\ )
		      step)
	      (setf temperature (* temperature cool))))))
    (print cnt)
    (print (funcall f i))
    i))

;;; ex06
(defun horner (x &rest coeffs)
  (let ((len (length coeffs)))
    (cond ((< len 1) nil)
	  ((= len 1) (car coeffs))
	  (t
	   (let ((res (+
		       (* (car coeffs) x)
		       (cadr coeffs)))
		 (morecoeffs (cddr coeffs)))
	     (dolist (coe morecoeffs res)
	       (setf res (+
			  (* res x)
			  coe))))))))
