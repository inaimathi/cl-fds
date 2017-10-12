(in-package #:cl-fds)

(defstruct rope (length 0) children)

(defun rope (&rest elems)
  (let ((v (coerce elems 'vector)))
    (make-rope :length (length v) :children (list v))))

(defmethod len ((r rope)) (rope-length r))

(defmethod cat ((ra rope) (rb rope))
  (make-rope :length (+ (len ra) (len rb)) :children (list ra rb)))

(defmethod catn ((r rope) &rest more-things)
  (let ((cs (cons r more-things)))
    (make-rope :length (loop for c in cs sum (len c)) :children cs)))

(defmethod insert-at ((thing rope) (ix integer) val) :todo)
(defmethod split-at ((thing rope) (ix integer)) :todo)

(defmethod ix ((r rope) (ix integer))
  (when (>= ix (len r)) (error "Invalid index error"))
  (let ((i ix))
    (loop for c in (rope-children r) for l = (len c)
       if (> l i) return (ix c i)
       else do (decf i l))))

(defmethod traverse! ((r rope) (fn function))
  (loop for c in (rope-children r)
     do (traverse! c fn)))

(defmethod traverse ((r rope) (fn function))
  (make-rope
   :length (len r)
   :children
   (loop for c in (rope-children r)
      collect (traverse c fn))))

(defmethod empty ((format (eql 'rope)))
  (make-rope :children nil))

(defmethod as ((format (eql 'list)) (r rope))
  (loop for c in (rope-children r) append (as format c)))
(defmethod as ((format (eql 'vector)) (r rope))
  (let ((new-arr (make-array (list (len r))))
	(next 0))
    (traverse!
     r (lambda (v)
	 (setf (aref new-arr next) v)
	 (incf next)))
    new-arr))
(defmethod as ((format (eql 'string)) (r rope))
  (let ((new-arr (make-string (len r)))
	(next 0))
    (traverse!
     r (lambda (v)
	 (setf (aref new-arr next) v)
	 (incf next)))
    new-arr))
