;;;; database.lisp

(in-package #:cloudfront-log-processor)

(defparameter *db* "quicklisp")
(defparameter *db-user* "quicklisp")
(defparameter *db-password* nil)
(defparameter *db-host* "localhost")

(defvar *db-connection* nil)

(defun db-connect ()
  (postmodern:connect *db* *db-user* *db-password* *db-host*))

(defun call-with-connection (fun)
  (if *db-connection*
      (let ((postmodern:*database* *db-connection*))
        (funcall fun))
      (unwind-protect
           (progn
             (setf *db-connection* (db-connect))
             (let ((postmodern:*database* *db-connection*))
               (funcall fun)))
        (when *db-connection*
          (postmodern:disconnect *db-connection*)
          (setf *db-connection* nil)))))

(defmacro with-connection (&body body)
  `(call-with-connection (lambda ()
                           ,@body)))
