(in-package #:cl-fds)

(defparameter *m* 4)
(defparameter *n* (- *m* 1))

(defstruct ribbit (size-table nil) (depth 0) (vec #()))

(defun take-across (depth rbs)
  (if (not (cdr rbs))
      (values (first rbs) nil)
      (let ((ct *m*)
	    (head nil)
	    (tail nil))
	(loop for rs on rbs while (> ct 1)
	   for r = (first rs)
	   for v = (ribbit-vec r)
	   do (cond ((> (length v) ct)
		     (push (subseq v 0 ct) head)
		     (setf tail (cons (mk-ribbit depth (subseq v ct)) (rest rs))
			   ct 0))
		    (t (push v head)
		       (decf ct (length v))))
	   finally (unless tail (setf tail rs)))
	(values
	 (mk-ribbit depth (apply #'concatenate 'vector (reverse head)))
	 tail))))

(defun repartition (depth rbs)
  (let ((rest rbs))
    (loop while rest for r = (pop rest)
       if (reusable-ribbit? r) collect r
       else collect (multiple-value-bind (next rst) (take-across depth (cons r rest))
		      (setf rest rst)
		      next))))

(defun full-level? (ribbit) (= *m* (length (ribbit-vec ribbit))))

(defun reusable-ribbit? (ribbit)
  (let* ((d (ribbit-depth ribbit))
	 (ct (length (ribbit-vec ribbit)))
	 (reusable-lv (or (= *m* ct) (= *n* ct))))
    (or (and (zerop d) reusable-lv)
	(let ((max (expt *m* (+ 1 d)))
	      (l (len ribbit)))
	  (or (= l max) (= l (- max 1))))
	(and reusable-lv
	     (every #'reusable-ribbit? (ribbit-vec ribbit))))))

(defun compute-size-table (depth vec)
  (unless (or (zerop depth) (and (= *m* (length vec)) (every #'full-level? vec)))
    (coerce
     (let ((s 0))
       (loop for e across vec
	  do (incf s (len e)) collect s))
     'vector)))

(defun mk-ribbit (depth vec)
  (make-ribbit :size-table (compute-size-table depth vec) :depth depth :vec vec))

(defun ribbit-level (depth elems)
  (let ((es elems))
    (loop while es
       collect (mk-ribbit
		depth (as 'vector (loop repeat *m* while es
				     for e = (pop es) collect e))))))

(defun naive-ribbit (depth elems)
  (let ((rbs (ribbit-level depth elems))
	(d depth))
    (loop while (cdr rbs)
       do (setf rbs (ribbit-level (incf d) rbs)))
    (first rbs)))

(defun ribbit (&rest elems)
  (if elems
      (naive-ribbit 0 elems)
      (make-ribbit)))

(defmethod len ((rb ribbit))
  (cond ((zerop (ribbit-depth rb))
	 (length (ribbit-vec rb)))
	((null (ribbit-size-table rb))
	 (expt *m* (+ 1 (ribbit-depth rb))))
	(t (let ((szs (ribbit-size-table rb)))
	     (aref szs (- (length szs) 1))))))

(defun raise-ribbit-to (depth ribbit)
  (if (>= (ribbit-depth ribbit) depth)
      ribbit
      (let ((r ribbit)
	    (d (ribbit-depth ribbit)))
	(loop until (= d depth)
	   do (setf r (mk-ribbit (incf d) (vector r))))
	r)))

(defun prune-ribbit-to (depth ribbit)
  (if (= depth (ribbit-depth ribbit))
      (list ribbit)
      (let ((rbs (list ribbit)))
	(loop until (= depth (ribbit-depth (first rbs)))
	   do (setf rbs (loop for r in rbs append (coerce (ribbit-vec ribbit) 'list))))
	rbs)))

(defun max-reusable-level (ribbit)
  (cond ((reusable-ribbit? ribbit) (ribbit-depth ribbit))
	((zerop (ribbit-depth ribbit)) nil)
	(t (loop for r across (ribbit-vec ribbit)
	      for d = (max-reusable-level r)
	      if d do (return d)))))

(defmethod cat ((a ribbit) (b ribbit))
  (let* ((d (ribbit-depth a))
	 (max-d (max-reusable-level a))
	 (reusable-a? (and max-d (= max-d d))))
    (cond ((and reusable-a? (>= d (ribbit-depth b)))
	   (mk-ribbit (+ d 1) (vector a (raise-ribbit-to d b))))
	  (reusable-a?
	   (naive-ribbit (+ d 1) (cons a (prune-ribbit-to d b))))
	  (max-d
	   (naive-ribbit
	    (+ max-d 1)
	    (repartition
	     max-d (append
		    (prune-ribbit-to max-d a)
		    (prune-ribbit-to
		     max-d (raise-ribbit-to max-d b))))))
	  (t
	   (naive-ribbit 1 (repartition 0 (cons a (prune-ribbit-to 0 b))))))))

(defmethod set! ((r ribbit) (ix integer) val) :todo)
(defmethod slice ((r ribbit) &key from to) :todo)
(defmethod insert-at ((rb ribbit) (ix integer) val) :todo)
(defmethod split-at ((rb ribbit) (ix integer)) :todo)
(defmethod ix ((rb ribbit) (ix integer)) :todo)
(defmethod traverse! ((r ribbit) fn) :todo)
(defmethod traverse ((r ribbit) fn) :todo)

(defmethod empty ((format (eql 'ribbit)))
  (make-ribbit))

(defmethod as ((format (eql 'ribbit)) thing) :todo)
