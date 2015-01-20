;;;; logging.lisp

(in-package #:cloudfront-log-processor)

(define-flag *log-file*
    :selector "log-file"
    :default-value nil
    :type (or null string)
    :documentation "The file to which activity should be logged.")

(defparameter *log-stream* (make-synonym-stream '*standard-output*))

(defun initialize-logging ()
  (cond ((null *log-file*)
         (setf *log-stream* (make-broadcast-stream)))
        ((equal *log-file* "-")
         (setf *log-stream* (make-synonym-stream '*standard-output*)))
        (t
         (setf *log-stream* (open *log-file*
                                  :direction :output
                                  :if-does-not-exist :create
                                  :if-exists :append)))))

(defun finish-logging ()
  (when *log-file*
    (finish-output *log-stream*)
    (close *log-stream*)))

(defmacro with-logging (&body body)
  `(progn
     (initialize-logging)
     (unwind-protect
          (progn ,@body)
       (finish-logging))))

(defun log-message (control &rest data)
  (if data
      (format *log-stream* "~{~}" control data)
      (write-string control *log-stream*)))
