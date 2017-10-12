(in-package #:cl-fds)

(defstruct mutation target k v)
(defstruct insertion target k v)
(defstruct slice target from to)
(defstruct deletion target k span)

(defmethod -mutate! ((s string) (k integer) (v character))
  )

(defmethod set! ((s string) (ix integer) (v character))
  (make-mutation :k ix :v v :target s))
