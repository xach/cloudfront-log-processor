;;;; import.lisp

(in-package #:cloudfront-log-processor)

;;; Fetch and import CloudFront log files

(defun rename-log-file (bucket old-key new-key)
  (zs3:copy-object :from-bucket bucket
                   :to-bucket bucket
                   :from-key old-key
                   :to-key new-key)
  (zs3:delete-object bucket old-key)
  t)

(defun mark-processed (bucket key)
  (let ((new-key (format nil "processed/~A" key)))
    (rename-log-file bucket key new-key)))

(defun call-for-each-object (bucket prefix fun)
  (let ((response (zs3:query-bucket bucket :prefix prefix)))
    (loop
      (if response
          (map nil (lambda (zs3-key)
                     (funcall fun (zs3:name zs3-key)))
               (zs3:keys response))
          (return))
      (setf response (zs3:continue-bucket-query response)))))

(defmacro for-each-object ((key &key bucket prefix) &body body)
  `(call-for-each-object ,bucket ,prefix (lambda (,key) ,@body)))

(defun process-cloudfront-logs (bucket prefix)
  (commando:in-temporary-directory
    (for-each-object (key :bucket bucket :prefix prefix)
      (let ((name (file-namestring key)))
        (log-message "~&; Processing ~A from ~A~%" key bucket)
        (finish-output)
        (zs3:get-file bucket key name)
        (process-log-file name)
        (mark-processed bucket key)
        (delete-file name)))))
