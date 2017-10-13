;;;; src/cl-fds.lisp

(in-package #:cl-fds)

(defgeneric len (thing))
(defgeneric cat (thing-a thing-b))
(defgeneric catn (thing &rest more-things))
(defgeneric set! (thing ix val))
(defgeneric slice (thing &key from to))
(defgeneric insert-at (thing ix val))
(defgeneric split-at (thing ix))
(defgeneric ix (thing ix))
(defgeneric traverse! (thing fn))
(defgeneric traverse (thing fn))
(defgeneric empty (format))
(defgeneric empty? (thing))
(defgeneric as (format thing))
