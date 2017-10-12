;;;; src/package.lisp

(defpackage #:cl-fds
  (:use #:cl)
  (:export #:len #:cat #:catn #:set! #:slice #:insert-at #:split-at #:ix #:traverse! #:traverse #:empty #:as

	   #:rope))
