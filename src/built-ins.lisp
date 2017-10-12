(in-package #:cl-fds)

(defmethod len ((s string)) (length s))
(defmethod len ((v vector)) (length v))
(defmethod len ((l list)) (length l))

;; TODO - cat

(defmethod catn (thing &rest more-things)
  (reduce #'cat more-things :initial-value thing))

;; TODO - set!
;; TODO - slice
;; TODO - insert-at
;; TODO - split-at

(defmethod ix ((s string) (ix integer)) (aref s ix))
(defmethod ix ((v vector) (ix integer)) (aref v ix))
(defmethod ix ((l list) (ix integer)) (nth ix l))

(defmethod traverse! ((l list) fn) (mapc fn l))
(defmethod traverse! ((v vector) fn) (loop for val across v do (funcall fn val)) nil)
(defmethod traverse! ((s string) fn) (loop for val across s do (funcall fn val)) nil)

(defmethod traverse ((l list) fn) (mapcar fn l))
(defmethod traverse ((v vector) fn) (map 'vector fn v))
(defmethod traverse ((s string) fn) (map 'string fn s))

(defmethod empty ((format (eql 'list))) nil)
(defmethod empty ((format (eql 'vector))) #())
(defmethod empty ((format (eql 'string))) "")

(defmethod as ((format (eql 'list)) (l list)) l)
(defmethod as ((format (eql 'list)) (v vector)) (coerce v 'list))
(defmethod as ((format (eql 'list)) (s string)) (coerce s 'list))
(defmethod as ((format (eql 'vector)) (v vector)) v)
(defmethod as ((format (eql 'vector)) (l list)) (coerce l 'vector))
(defmethod as ((format (eql 'vector)) (s string)) (coerce s 'vector))
(defmethod as ((format (eql 'string)) (s string)) s)
(defmethod as ((format (eql 'string)) (v vector)) (coerce v 'string))
(defmethod as ((format (eql 'string)) (l list)) (coerce l 'string))
