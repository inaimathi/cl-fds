(in-package #:cl-fds)

(defparameter *ribbit-size* 4)

(defstruct ribbit (size-table nil) (depth 0) (vec #()))

(defun take-across (vecs)
  (let ((ct *ribbit-size*)
	(head nil)
	(tail nil))
    (loop for vs on vecs while (> ct 1)
       for v = (typecase (first vs)
		 (ribbit (ribbit-vec (first vs)))
		 (t (first vs)))
       do (cond ((> (length v) ct)
		 (push (subseq v 0 ct) head)
		 (setf tail (cons (subseq v ct) (rest vs))
		       ct 0))
		(t (push v head)
		   (decf ct (length v))))
       finally (unless tail (setf tail vs)))
    (values
     (let ((v (first head)))
       (cond ((and (not (cdr head))
		   (or (ribbit-p v) (vectorp v)))
	      v)
	     (t (apply #'concatenate 'vector (reverse head)))))
     tail)))

(defun repartition (rbs)
  (let ((rest rbs))
    (loop while rest
       collect (multiple-value-bind (next rst) (take-across rest)
		 (setf rest rst)
		 next))))

;; (defun compute-size-table (depth vec)
;;   (unless (or (zerop depth) (and (= *ribbit-size* (length vec)) (every #'full-level? vec)))
;;     (coerce
;;      (let ((s 0))
;;        (loop for e across vec
;; 	  do (incf s (len e)) collect s))
;;      'vector)))

(defmethod len ((rb vector)) (length rb))
(defmethod len ((rb ribbit))
  (cond ((zerop (ribbit-depth rb))
	 (length (ribbit-vec rb)))
	((null (ribbit-size-table rb))
	 (expt *ribbit-size* (+ 1 (ribbit-depth rb))))
	(t (let ((szs (ribbit-size-table rb)))
	     (aref szs (- (length szs) 1))))))

(defmethod empty ((format (eql 'ribbit)))
  (make-ribbit))

;; (defmethod coerce-to ((lst list) (format (eql 'ribbit)))
;;   (if lst

;;       (empty format)))
