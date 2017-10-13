;;;; test/cl-fds.lisp

(in-package #:cl-fds-test)

;; (tests
;;  (is (+ 2 3) 5 "Addition works")
;;  (is (+ 2 3) 6 "Intentionally fails")

;;  (for-all ((a a-number) (b a-number))
;; 	  (is= (+ a b) (+ b a))
;; 	  "Addition is commutative")
;;  (for-all ((a a-number) (b a-number))
;; 	  (is= (- a b) (- b a))
;; 	  "Subtraction is not, so this should fail"))

(let ((a (ribbit 1 2 3 4))
      (b (ribbit 5 6 7 8))
      (c (ribbit 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
      (d (cat (ribbit 1 2 3 4 5 6 7) (ribbit 8 9 10 11 12 13 14))))
  (tests
   (is t (and (eq a (ix (cl-fds::ribbit-vec (cat a b)) 0))
	      (eq b (ix (cl-fds::ribbit-vec (cat a b)) 1)))
       "cat on ribbits shares reusable leaf pointers")
   (is t (eq c (ix (cl-fds::ribbit-vec (cat c a)) 0))
       "cat on ribbits shares reusable deeper pointers on the left tree")
   (is t (eq (ix (cl-fds::ribbit-vec d) 2)
	     (ix (cl-fds::ribbit-vec (cat (ribbit 1) d)) 2))
       "cat on ribbits maximally reuses in even in the screw case")))
