(asdf:defsystem simple-flow-dispatcher
  :description "Reference implementation of a dispatcher for cl-flow library"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (alexandria cl-muth bodge-queue)
  :serial t
  :components ((:file "packages")
               (:file "dispatcher")))
