;;;; cl-fds.asd

(asdf:defsystem #:cl-fds
  :description "Describe cl-fds here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:module
		src :components
		((:file "package")
		 (:file "cl-fds")
		 (:file "rope")
		 (:file "ribbit")))))

(asdf:defsystem #:cl-fds-test
  :description "Test suite for :cl-fds"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:cl-fds #:test-utils)
  :defsystem-depends-on (#:prove-asdf)
  :components ((:module
                test :components
                ((:file "package")
                 (:test-file "cl-fds"))))
  :perform (test-op
	    :after (op c)
	    (funcall (intern #.(string :run) :prove) c)))
