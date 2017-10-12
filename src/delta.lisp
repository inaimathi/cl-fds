(in-package #:cl-fds)

;;;;; Model definition and internals
(defclass delta ()
  ((target :accessor target :initarg :target)
   (forced? :accessor forced? :initform nil)
   (len :reader len :initarg :len :initform 0)
   (empty :reader empty :initarg :empty)
   (depth :reader depth :initarg :depth :initform 0)))
(defclass mutation (delta) ((changes :reader changes :initarg :changes)))
(defclass insertion (delta) ((k :reader k :initarg :k) (v :reader v :initarg :v)))
(defclass slice (delta) ((from :reader from :initarg :from) (to :reader to :initarg :to)))

(defmethod apply!! ((tgt vector) (m mutation))
  (loop for (k . v) in (changes m)
     do (setf (aref tgt k) v))
  tgt)
(defmethod apply!! ((tgt list) (m mutation))
  (loop for elem in tgt for i from 0
     for new = (cdr (assoc i (changes m)))
     if new collect new else collect elem))

(defmethod apply!! (tgt (s slice))
  (subseq tgt (from s) (to s)))

;; TODO - optimize collapse! and apply!! for insertion
;; NOTE - There are probably better macro-level optimizations we could run here. In particular, it's pointless to keep anything but the latest slice for a delta. Similarly, we can cluster mutations, both discarding ones that get sliced out of bounds and overriding ones that hit the same coordinate as a previous mutation. Insertions are the only thing that really act kinda weirdly. We can't reasonably cluster them, though they are expressible as cat/slice combos if we go that route. Anyway, interaction between insertion and slice/mutation are the only things keeping this from being a trivial problem. If we can resolve that (and I'm leaning towards a thing that looks like operational transformations), we should get a data structure that has good asymptotic and constant factors (unlike the below, which is currently about as bad as the naive approach, albeit amortized, and has some pretty horrendous constant factors attached thanks to method call overhead).
;; NOTE - These are the ways we could optimize
;;     1. Re-model deltas so that they're an initial target and a list of transformations on it. When adding a new transformation, condense existing ones instead of just wrapping the old one as a target. (So, slices discard old slices and remove out-of-bounds mutations/insertions, mutations still override earlier mutations at the same index, insertions affect mutations and slices that come after them)
;;     2. Add `cat` as a separate "delta". Add a corresponding apply!! and collapse! implementation, and make sure that the other delta generating functions do the appropriate thing when dealing with cat inputs.

(defmethod apply!! ((tgt vector) (ins insertion))
  (concatenate 'vector (subseq tgt 0 (k ins)) (v ins) (subseq tgt (k ins))))
(defmethod apply!! ((tgt list) (ins insertion))
  (concatenate 'list (subseq tgt 0 (k ins)) (v ins) (subseq tgt (k ins))))

(defmethod collapse! (thing) thing)
(defmethod collapse! ((d delta))
  (if (forced? d)
      (target d)
      (let ((tgt (target d))
	    (transforms (list d)))
	(loop while (typep tgt 'delta)
	   do (progn (push tgt transforms)
		     (setf tgt (target tgt))))
	(when (vectorp tgt) (setf tgt (copy-seq tgt)))
	(loop for tr in transforms
	   do (setf tgt (apply!! tgt tr)))
	(setf (forced? d) t
	      (target d) tgt)
	tgt)))

;;;;; Basic methods for the built-ins and deltas
(defmethod cat ((a delta) (b delta))
  (cat (collapse! a) (collapse! b)))
(defmethod catn ((d delta) &rest more-deltas)
  (apply #'catn (mapcar #'collapse! (cons d more-deltas))))

(defmethod set! ((d delta) (ix integer) val)
  (if (forced? d)
      (set! (target d) ix val)
      (make-instance 'mutation :depth (+ (depth d)) :len (len d) :target d :changes (list (cons ix val)) :empty (empty d))))
(defmethod set! ((m mutation) (ix integer) val)
  (make-instance 'mutation :depth (depth m) :len (len m) :target (target m) :empty (empty m)
		 :changes (cons (cons ix val)
				(remove-if
				 (lambda (change) (= ix (car change)))
				 (changes m)))))
(defmethod set! ((s string) (ix integer) (val character))
  (make-instance 'mutation :len (length s) :target s :changes (list (cons ix val)) :empty ""))
(defmethod set! ((v vector) (ix integer) val)
  (make-instance 'mutation :len (length v) :target v :changes (list (cons ix val)) :empty #()))
(defmethod set! ((l list) (ix integer) val)
  (make-instance 'mutation :len (length l) :target l :changes (list (cons ix val)) :empty nil))

(defun mk-slice (tgt from to zero depth)
  (assert (>= from 0) nil ":from must be 0 or greater")
  (unless (listp tgt)
    (assert (or (null to) (>= (len tgt) to)) nil ":to must either be nil or no greater than the size of the target"))
  (cond ((and (zerop from) (null to)) tgt)
	((and to (= from to)) (empty tgt) zero)
	(t (make-instance 'slice :depth depth :len (- (or to (len tgt)) from) :target tgt :from from :to to :empty zero))))
(defmethod slice ((d delta) &key (from 0) to)
  (if (forced? d)
      (slice (target d) :from from :to to)
      (mk-slice d from to (empty d) (+ 1 (depth d)))))
(defmethod slice ((s slice) &key (from 0) (to (to s)))
  (let ((+from (+ (from s) from))
	(+to (min to (to s))))
    (mk-slice (target s) +from +to (empty s) (depth s))))
(defmethod slice ((v vector) &key (from 0) to)
  (mk-slice v from to #() 0))
(defmethod slice ((s string) &key (from 0) to)
  (mk-slice s from to "" 0))
(defmethod slice ((l list) &key (from 0) to)
  (mk-slice l from to nil 0))

(defmethod insert-at ((d delta) (ix integer) wedge)
  (if (forced? d)
      (insert-at (target d) ix wedge)
      (make-instance 'insertion :depth (+ 1 (depth d)) :len (+ (len d) (len wedge)) :target d :k ix :v wedge :empty (empty d))))
(defmethod insert-at ((s string) (ix integer) (wedge string))
  (make-instance 'insertion :len (+ (length s) (length wedge)) :target s :k ix :v wedge :empty ""))
(defmethod insert-at ((v vector) (ix integer) (wedge vector))
  (make-instance 'insertion :len (+ (length v) (length wedge)) :target v :k ix :v wedge :empty #()))
(defmethod insert-at ((l list) (ix integer) (wedge list))
  (loop for i from 0 for elem in l
     when (= i ix) append wedge
     collect elem))

(defun slices (thing ix) (values (slice thing :to ix) (slice thing :from ix)))
(defmethod split-at ((d delta) (ix integer))
  (if (forced? d) (split-at (target d) ix) (slices d ix)))
(defmethod split-at ((s string) (ix integer)) (slices s ix))
(defmethod split-at ((v vector) (ix integer)) (slices v ix))
(defmethod split-at ((l list) (ix integer)) (slices l ix))

(defmethod ix ((d delta) (ix integer)) (ix (collapse! d) ix))
(defmethod traverse! ((d delta) fn) (traverse! (collapse! d) fn))
(defmethod traverse ((d delta) fn) (traverse (collapse! d) fn))

(defmethod as ((format (eql 'list)) (d delta))
  (coerce (collapse! d) 'list))
(defmethod as ((format (eql 'string)) (d delta))
  (coerce (collapse! d) 'string))
(defmethod as ((format (eql 'vector)) (d delta))
  (coerce (collapse! d) 'vector))
