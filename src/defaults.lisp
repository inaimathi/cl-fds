(in-package #:cl-fds)

;; This file contains the default, type agnostic (and generally VERY sub-optimally
;; performing) implementations for some API methods. If you can make a more specialized
;; version, it is generally a good idea to do so, but some of these are either passably
;; performant, unimportant for some datatypes, or sadly infeasable at better performance.

(defmethod catn (thing &rest more-things)
  (reduce #'cat more-things :initial-value thing))

(defmethod ix (thing (ix integer))
  (nth ix (as 'list thing)))

(defmethod traverse! (thing fn)
  (mapc fn (as 'list thing)))

(defmethod empty? (thing)
  (zerop (len thing)))
