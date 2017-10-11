;;;; src/cl-fds.lisp

(in-package #:cl-fds)

(defgeneric len (thing))
(defmethod len ((s string)) (length s))
(defmethod len ((v vector)) (length v))
(defmethod len ((l list)) (length l))

(defgeneric cat (thing-a thing-b))
(defgeneric catn (thing &rest more-things))
(defmethod catn (thing &rest more-things)
  (reduce #'cat more-things :initial-value thing))

(defgeneric insert-at (thing ix val))
(defgeneric split-at (thing ix))
(defgeneric ix (thing ix))
(defmethod ix ((s string) (ix integer)) (aref s ix))
(defmethod ix ((v vector) (ix integer)) (aref v ix))
(defmethod ix ((l list) (ix integer)) (nth ix l))

(defgeneric traverse! (thing fn))
(defmethod traverse! ((l list) fn) (mapc fn l))
(defmethod traverse! ((v vector) fn) (loop for val across v do (funcall fn val)) nil)
(defmethod traverse! ((s string) fn) (loop for val across s do (funcall fn val)) nil)

(defgeneric traverse (thing fn))
(defmethod traverse ((l list) fn) (mapcar fn l))
(defmethod traverse ((v vector) fn) (map 'vector fn v))
(defmethod traverse ((s string) fn) (map 'string fn s))

(defgeneric empty (format))
(defmethod empty ((format (eql 'list))) nil)
(defmethod empty ((format (eql 'vector))) #())
(defmethod empty ((format (eql 'string))) "")

(defgeneric coerce-to (thing format))
(defmethod coerce-to ((l list) (format (eql 'list))) l)
(defmethod coerce-to ((v vector) (format (eql 'list))) (coerce v 'list))
(defmethod coerce-to ((s string) (format (eql 'list))) (coerce s 'list))
