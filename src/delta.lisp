(in-package #:cl-fds)

(defclass delta ()
  ((target :reader target :initarg :target)
   (len :reader len :initarg :len :initform 0)))
(defclass mutation (delta) ((changes :reader changes :initarg :changes)))
(defclass insertion (delta) ((k :reader k :initarg :k) (v :reader v :initarg :v)))
(defclass slice (delta) ((from :reader from :initarg :from) (to :reader to :initarg :to)))
;; (defclass deletion (delta) ((k :reader k :initarg :k) (span :reader span :initarg :size)))

;; TODO - majorly optimize apply!! and collapse!
(defmethod apply!! ((tgt vector) (m mutation))
  (loop for (k . v) in (changes m)
     do (setf (aref tgt (k m)) (v m))))
(defmethod apply!! ((tgt list) (m mutation))
  (loop for elem in tgt for i from 0
     for new = (cdr (assoc i (changes m)))
     if new collect new else collect elem))

(defmethod apply!! ((tgt vector) (ins insertion))
  (catn (subseq tgt 0 (k ins)) (v ins) (subseq tgt (k ins))))
(defmethod apply!! ((tgt list) (ins insertion))
  (catn (subseq tgt (k ins)) (v ins) (subseq tgt (k ins))))

(defmethod apply!! (tgt (s slice))
  (subseq tgt (from s) (to s)))

;; (defmethod apply!! (tgt (d deletion))
;;   (cat (subseq tgt 0 (from d)) (subseq tgt (+ (span d) (from d)))))

(defmethod collapse! (thing) thing)
(defmethod collapse! ((d delta))
  (let ((tgt (target d))
	(transforms (list d)))
    (loop while (typep tgt 'delta)
       do (progn (push tgt transforms)
		 (setf tgt (target d))))
    (when (vectorp tgt) (setf tgt (copy-seq tgt)))
    (loop for tr in transforms
       do (setf tgt (apply!! tgt tr)))
    tgt))

(defmethod set! ((m mutation) (ix integer) val)
  (make-instance 'mutation :len (len m) :target (target m) :changes (cons (cons ix val) (changes m))))
(defmethod set! ((s string) (ix integer) (val character))
  (make-instance 'mutation :len (length s) :target s :changes (list (cons ix val))))
(defmethod set! ((v vector) (ix integer) val)
  (make-instance 'mutation :len (length v) :target v :changes (list (cons ix val))))
(defmethod set! ((l list) (ix integer) val)
  (make-instance 'mutation :len (length l) :target l :changes (list (cons ix val))))

;; TODO - bounds check on from and to here
(defmethod slice ((s slice) &key (from 0) (to (to s)))
  (let ((+from (+ (from s) from))
	(+to (min to (to s))))
    (make-instance 'slice :target (target s) :len (- +to +from) :from +from :to +to)))
(defmethod slice ((v vector) &key (from 0) (to (length v)))
  (cond ((and (zerop from) (= (length v) to)) v)
	((= from to) #())
	(t (make-instance 'slice :len (- to from) :target v :from from :to to))))
(defmethod slice ((s string) &key (from 0) (to (length s)))
  (cond ((and (zerop from) (= (length s) to)) s)
	((= from to) "")
	(t (make-instance 'slice :len (- to from) :target s :from from :to to))))
(defmethod slice ((l list) &key (from 0) to)
  (let ((f (or from 0)))
    (if to
	(loop for elem in l for i from 0
	   when (and (> to i) (>= i f)) collect elem)
	(loop for elem in l for i from 0
	   when (>= i f) collect elem))))

(defmethod insert-at ((s string) (ix integer) (wedge string))
  (make-instance 'insertion :len (+ (length s) (length wedge)) :target s :k ix :v wedge))
(defmethod insert-at ((v vector) (ix integer) (wedge vector))
  (make-instance 'insertion :len (+ (length v) (length wedge)) :target v :k ix :v wedge))
(defmethod insert-at ((l list) (ix integer) (wedge list))
  (loop for i from 0 for elem in l
     when (= i ix) append wedge
     collect elem))

;; (defun compute-final-size (target transforms)
;;   (let* ((l (len target)))
;;     (loop for tr in transforms
;;        do (typecase tr
;; 	    (insertion (incf l (insertion-v tr)))
;; 	    (deletion (decf l (deletion-span tr)))
;; 	    (slice (setf l (- (or (slice-to tr) (- l 1)) (slice-from tr))))))
;;     l))
