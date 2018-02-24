(cl:in-package :simple-flow-dispatcher)


(define-constant +cas-sleep+ (/ 1 internal-time-units-per-second))


(defmacro %compare-and-swap (place old new)
  "Tries to atomically set a value of PLACE to be NEW if it
was EQ to OLD, returning non-nil if successful."
  #+lispworks `(sys:compare-and-swap ,place ,old ,new)
  #+ccl `(ccl::conditional-store ,place ,old ,new)
  #+sbcl (once-only (old)
           `(eq ,old (sb-ext:compare-and-swap ,place ,old ,new)))
  #-(or lispworks ccl sbcl) (error "fixme; implement CAS"))


(defstruct spin-lock
  (owned nil :type t))


(defun acquire-spin-lock (spin-lock)
  (loop until (%compare-and-swap (spin-lock-owned spin-lock) nil t)
     do (sleep +cas-sleep+)))


(defun release-spin-lock (spin-lock)
  (loop until (%compare-and-swap (spin-lock-owned spin-lock) t nil)
     do (sleep +cas-sleep+)))


(defmacro with-spin-lock-held ((spin-lock) &body body)
  (once-only (spin-lock)
    `(unwind-protect
          (progn
            (acquire-spin-lock ,spin-lock)
            ,@body)
       (release-spin-lock ,spin-lock))))
