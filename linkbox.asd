(asdf:defsystem :linkbox
  :description "A very simple URL shortener and file upload box."
  :author "Joram Schrijver <i@joram.io>"
  :license "MIT"
  :depends-on (#:hunchentoot
               #:alexandria)
  :components ((:file "server")))
