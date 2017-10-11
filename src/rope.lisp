(in-package #:cl-fds)

(defstruct rope (length 0) children)

(defun rope (&rest elems)
  (make-rope :length (length elems) :children (list (coerce elems 'vector))))

(defmethod len ((r rope)) (rope-length r))

(defmethod cat (a (rb rope))
  (make-rope :length (+ (len a) (len rb)) :children (list a rb)))
(defmethod cat ((ra rope) b)
  (make-rope :length (+ (len ra) (len b)) :children (list ra b)))
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

(defmethod coerce-to ((r rope) (format (eql 'list)))
  (loop for c in (rope-children r) append (coerce-to c format)))
(defmethod coerce-to ((r rope) (format (eql 'vector)))
  (let ((new-arr (make-array (list (len r))))
	(next 0))
    (traverse!
     r (lambda (v)
	 (setf (aref new-arr next) v)
	 (incf next)))
    new-arr))
(defmethod coerce-to ((r rope) (format (eql 'string)))
  (let ((new-arr (make-string (len r)))
	(next 0))
    (traverse!
     r (lambda (v)
	 (setf (aref new-arr next) v)
	 (incf next)))
    new-arr))
