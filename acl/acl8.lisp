;;; ex04
(defpackage "RING"
  (:use "COMMON-LISP")
  (:export "NEW-BUF" "BUF-INSERT" "BUF-POP" "BUF-NEXT"
	   "BUF-RESET" "BUF-CLEAR" "BUF-FLUSH"))

(defpackage "FILE"
  (:use "COMMON-LISP" "RING")
  (:export "FILE-SUBST"))

(in-package ring)

(defstruct buf
  vec (start -1) (used -1) (new -1) (end -1))

(defun bref (buf n)
  (svref (buf-vec buf)
         (mod n (length (buf-vec buf)))))

(defun (setf bref) (val buf n)
  (setf (svref (buf-vec buf)
               (mod n (length (buf-vec buf))))
        val))

(defun new-buf (len)
  (make-buf :vec (make-array len)))

(defun buf-insert (x b)
  (setf (bref b (incf (buf-end b))) x))

(defun buf-pop (b)
  (prog1 
    (bref b (incf (buf-start b)))
    (setf (buf-used b) (buf-start b)
          (buf-new  b) (buf-end   b))))

(defun buf-next (b)
  (when (< (buf-used b) (buf-new b))
    (bref b (incf (buf-used b)))))

(defun buf-reset (b)
  (setf (buf-used b) (buf-start b)
        (buf-new  b) (buf-end   b)))

(defun buf-clear (b)
  (setf (buf-start b) -1 (buf-used  b) -1
        (buf-new   b) -1 (buf-end   b) -1))

(defun buf-flush (b str)
  (do ((i (1+ (buf-used b)) (1+ i)))
      ((> i (buf-end b)))
    (princ (bref b i) str)))

(in-package file)

(defun file-subst (old new file1 file2)
  (with-open-file (in file1 :direction :input)
     (with-open-file (out file2 :direction :output
                                :if-exists :supersede)
       (stream-subst old new in out))))

(defun stream-subst (old new in out)
  (let* ((pos 0)
         (len (length old))
         (buf (new-buf len))
         (from-buf nil))
    (do ((c (read-char in nil :eof)
            (or (setf from-buf (buf-next buf))
                (read-char in nil :eof))))
        ((eql c :eof))
      (cond ((char= c (char old pos))
             (incf pos)
             (cond ((= pos len)            ; 3
                    (princ new out)
                    (setf pos 0)
                    (buf-clear buf))
                   ((not from-buf)         ; 2
                    (buf-insert c buf))))
            ((zerop pos)                   ; 1
             (princ c out)
             (when from-buf
               (buf-pop buf)
               (buf-reset buf)))
            (t                             ; 4
             (unless from-buf
               (buf-insert c buf))
             (princ (buf-pop buf) out)
             (buf-reset buf)
             (setf pos 0))))
    (buf-flush buf out)))

(in-package common-lisp-user)

;;; ex05
(defun in (item lst)
  (if (null lst)
      nil
      (if (eql item (car lst))
	  (return-from in t)
	  (in item (cdr lst)))))

(defun space-p (c)
  (in c '(#\  #\Tab #\Newline)))

(defun punc (c)
  (case c
    (#\. '|.|) (#\, '|,|) (#\; '|;|) 
    (#\! '|!|) (#\? '|?|) (#\: '|:|)))

(defun generated-text? (text)
  (let ((pos 0)
	(len (length text)))
    (labels ((read-word-or-punc ()
	       (if (< pos len)
		   (let ((c (char text pos))
			 (p nil))
		     (cond
		       ((char= #\space c)
			;; skip preceding spaces and read again
			(setf pos (position-if #'(lambda (c)
						   (not (char= c #\space)))
					       text
					       :start pos))
			(read-word-or-punc))
		       ((setf p (punc c))
			(incf pos)
			p)
		       (t
			(make-symbol (string-downcase
				      (subseq text pos
					      (position-if #'(lambda (c)
							       (or (space-p c)
								   (punc c)))
							   text
							   :start pos)))))))
		   ;; whole string read, return nil
		   nil)))
      (do ((prev (read-word-or-punc) next)
	   (next (read-word-or-punc) (read-word-or-punc)))
	  ((null prev) t)
	(format t "~A" prev)
	(let ((pair (assoc next (gethash prev *words*))))
	  (if (null pair) 
	      (return-from generated-text? nil)))
	(setf prev next)))))

;;; ex06
(defconstant maxword 100)
(defconstant max-num-words 10000)

(defparameter *words* (make-hash-table :size max-num-words))
(defparameter *sword* (make-hash-table :size max-num-words))

(defparameter *buf* (make-string maxword))

(let ((tmp (cons nil nil)))
  (defun my-read-char (str &rest args)
    (if (cdr tmp)
	(progn
	  (setf (cdr tmp) nil)
	  (car tmp))
	(setf (car tmp) (apply #'read-char str args))))
  (defun my-putback-char (&optional c)
    (if c
	(setf (car tmp) c))
    (setf (cdr tmp) T)
    (car tmp))
  (defun my-last-read ()
    (car tmp)))

(defun read-until (str &optional (test #'space-p))
  (do ((c (my-read-char str nil :eof)
	  (my-read-char str nil :eof))
       (pos 0 (incf pos)))
      ((or (funcall test c) (eql c :eof))
       (values (subseq *buf* 0 pos)
	       (eql c :eof)))
    (setf (aref *buf* pos) c)))

(defun read-word (str)
  (read-until str
	      #'(lambda (c)
		  (not (space-p c))))
  (my-putback-char)
  (read-until str 
	      #'(lambda (c)
		  (or (punc c)
		      (space-p c)))))

(defun read-text (pathname)
  (with-open-file (s pathname :direction :input)
    (do ((i 0 (incf i)))
	((= i max-num-words))
      (multiple-value-bind (w eof?) (read-word s)
	(when (> (length w) 0)
	  (saw (see (intern (string-downcase w))))
	  (let ((p (punc (my-last-read))))
	    (if p (saw (see p))))
	  (if eof? (return)))))))
  
(let ((prev '|.|))
  (defun see (symb)
    (let ((pair (assoc symb (gethash prev *words*))))
      (if (null pair)
          (push (cons symb 1) (gethash prev *words*))
          (incf (cdr pair))))
    (setf prev symb)))

(let ((prev nil))
  (defun saw (symb)
    (if prev
	(let ((pair (assoc prev (gethash symb *sword*))))
	  (if (null pair)
	      (push (cons prev 1) (gethash symb *sword*))
	      (incf (cdr pair)))))
    (setf prev symb)))

(defun generate-text (n &optional (prev '|.|))
  (if (zerop n)
      (terpri)
      (let ((next (random-next prev)))
        (format t "~A " next)
        (generate-text (1- n) next))))

(defun random-next (prev &optional (direction 'n))
  (let* ((tab (if (eql direction 'n)
		  *words*
		  *sword*))
	 (choices (gethash prev tab))
         (i (random (reduce #'+ choices 
                            :key #'cdr))))
    (dolist (pair choices)
      (if (minusp (decf i (cdr pair)))
          (return (car pair))))))

(defun centered-sentence (center-word &optional n)
  (if (and n (> n 0))
      (setf n (floor (/ n 2)))
      (setf n nil))
  (let ((sentence nil)
	(prev center-word)
	(i 0))
    (do ((next (random-next prev 'p) (random-next prev 'p)))
	 ((if n
	      (= i n)
	      (punc (char (symbol-name next) 0))))
      (push next sentence)
      (setf prev next)
      (incf i))
    (when sentence
      (format t "~A" (car sentence))
      (dolist (word (cdr sentence))
	(if (punc (char (symbol-name word) 0))
	    (format t "~A" word)
	    (format t " ~A" word))))
    (format t " ~A " center-word)
    (generate-text (or n i) center-word)))