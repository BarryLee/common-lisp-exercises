;;; ex01
(defun lines (filename)
  (let ((acc nil))
    (with-open-file (f filename :direction :input)
      (do ((line (read-line f nil 'eof)
		 (read-line f nil 'eof)))
	  ((eql line 'eof) (nreverse acc))
	(push line acc)))))

;;; ex02
(defun expressions (filename)
  (let ((acc nil))
    (with-open-file (f filename :direction :input)
      (do ((exp (read f nil 'eof)
		(read f nil 'eof)))
	  ((eql exp 'eof) (nreverse acc))
	(push exp acc)))))

;;; ex03
(defun de-comment (infile outfile)
  (with-open-file (in-str infile :direction :input)
    (with-open-file (out-str outfile :direction :output
			     :if-exists :supersede)
      (do ((line (read-line in-str nil 'eof)
		 (read-line in-str nil 'eof)))
	  ((eql line 'eof))
	(let ((comment-begins (position #\% line))
	      (content-begins (position-if #'(lambda (x)
					       (and
						(graphic-char-p x)
						(not (eql x #\ ))))
					   line)))
	  (cond ((not comment-begins) (format out-str "~A~%" line))
		((> comment-begins content-begins)
		 (format out-str "~A~%" (subseq line 0 comment-begins)))
		(t nil)))))))

;;; ex04
(defun print-matrix (m)
  (let ((x (array-dimension m 0))
	(y (array-dimension m 1)))
    (do ((i 0 (incf i)))
	((>= i x))
      (do ((j 0 (incf j)))
	  ((>= j y))
	(format t "~10,2F" (aref m i j)))
      (format t "~%"))))

;;; ex05
(defun stream-subst (old new in out)
  (let* ((pos 0)
         (len (length old))
         (buf (new-buf len))
         (from-buf nil))
    (do ((c (read-char in nil :eof)
            (or (setf from-buf (buf-next buf))
                (read-char in nil :eof))))
        ((eql c :eof))
      (cond ((or (char= c (char old pos))
		 (char= (char old pos) #\+))
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

;;; ex06
(defun stream-subst (old new in out)
  (let ((pos 0)
	(len (length old))
	(from-buf nil)
	(pattern nil))
    (do ((i 0 (incf i)))
	((= i len))
      (let ((c (char old i)))
	(if (char= c #\%)
	    (push (char old (incf i)) pattern)
	    (cond ((char= c #\$)
		   (push '$ pattern))
		  ((char= c #\@)
		   (push '@ pattern))
		  ((char= c #\+)
		   (push '+ pattern))
		  (t
		   (push c pattern))))))
    (setf len (length pattern)
	  pattern (make-array len
			      :initial-contents (nreverse pattern)))
    (print pattern)
    (let ((buf (new-buf (length pattern))))
      (do ((c (read-char in nil :eof)
	      (or (setf from-buf (buf-next buf))
		  (read-char in nil :eof)))
	   (pc (aref pattern pos) (aref pattern pos)))
	  ((eql c :eof))
	(cond ((or (eql pc '+)
		   (and (eql pc '$)
			(digit-char-p c))
		   (and (eql pc '@)
			(alpha-char-p c))
		   (eql c pc))
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
      (buf-flush buf out))))
