(cl:defpackage :simple-flow-dispatcher
  (:use :cl :alexandria :bodge-queue)
  (:export make-simple-dispatcher
           free-simple-dispatcher))
