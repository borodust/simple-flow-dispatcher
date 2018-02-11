(asdf:defsystem simple-flow-dispatcher
  :description "Reference implementation of a dispatcher for cl-flow library"
  :version "0.0.1"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (alexandria cl-muth)
  :serial t
  :components ((:file "packages")
               (:file "atomic")
               (:file "dispatcher")))
