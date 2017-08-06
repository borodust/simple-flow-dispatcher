(in-package :simple-flow-dispatcher)


(declaim (special *dispatcher*))


(defclass tagged-queue ()
  ((lock :initform (make-spin-lock))
   (queue-table :initform (make-hash-table :test 'eq))))


(defun push-task (queue tag task)
  (with-slots (queue-table lock) queue
    (with-spin-lock-held (lock)
      (let ((queue (gethash tag queue-table)))
        (if (null queue)
            (prog1 nil
              (push task (gethash tag queue-table)))
            (prog1 (first queue)
              (push task (cdr queue))))))))


(defun pop-task (queue tag)
  (with-slots (queue-table lock) queue
    (with-spin-lock-held (lock)
      (multiple-value-bind (value present-p) (gethash tag queue-table)
        (when present-p
          (if (null (rest value))
              (remhash tag queue-table)
              (setf (gethash tag queue-table) (rest value)))
          (first value))))))


(defun peek-task (queue tag)
  (with-slots (queue-table lock) queue
    (with-spin-lock-held (lock)
      (first (gethash tag queue-table)))))


(defun clear-tagged-queue (queue)
  (with-slots (queue-table lock) queue
    (with-spin-lock-held (lock)
      (clrhash queue-table))))


;;;
;;;
;;;
(defclass simple-dispatcher ()
  ((tasks :initform (make-instance 'tagged-queue))
   (error-handler :initform nil :initarg :error-handler)
   pool))


(defmethod initialize-instance :after ((this simple-dispatcher) &key threads)
  (with-slots (pool kernel channel queue) this
    (setf pool (mt:make-thread-pool threads))
    (mt:open-pool pool)))


(defun dispatch-with (dispatcher fn invariant)
  (with-slots (pool tasks error-handler) dispatcher
    (prog1 nil
      (labels ((handle-task (task)
                 (handler-bind ((simple-error (lambda (e)
                                                (when error-handler
                                                  (handler-case
                                                      (funcall error-handler e)
                                                    (t ())))
                                                (return-from handle-task))))
                   (let ((*dispatcher* dispatcher))
                     (funcall task))))
               (handle-tasks ()
                 (loop for task = (peek-task tasks invariant)
                    while task
                    do (progn
                         (handle-task task)
                         (pop-task tasks invariant)))))
        (unless (push-task tasks invariant fn)
          (mt:push-to-pool pool #'handle-tasks))))))


(defun make-simple-dispatcher (&key (threads 2) error-handler)
  "Makes simple thread-safe cl-flow dispatcher that can handle single invariants. For invariants
to be considered the same they must be EQ. For example:

(-> :guarded ()
  (do-some-work))

While this flow block is running other blocks with the same invariant (EQ to :guarded) will
never be executed concurrently."
  (let ((dispatcher (make-instance 'simple-dispatcher
                                   :threads threads
                                   :error-handler error-handler)))
    (lambda (fn invariant &key &allow-other-keys)
      (dispatch-with dispatcher fn invariant))))


(defun free-simple-dispatcher (simple-dispatcher)
  "Release resources acquired by the dispatcher"
  (flet ((shutdown-dispatcher ()
           (with-slots (pool tasks) *dispatcher*
             (mt:close-pool pool)
             (clear-tagged-queue tasks))))
    (funcall simple-dispatcher #'shutdown-dispatcher nil)))
