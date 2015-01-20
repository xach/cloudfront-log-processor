;;;; cloudfront-log-processor.lisp

(in-package #:cloudfront-log-processor)

;;; "cloudfront-log-processor" goes here. Hacks and glory await!

(defun process-log-file (file)
  (with-connection
    (postmodern:ensure-transaction
      (unless (file-processed-p file)
        (let ((file-id
               (postmodern:query
                (:insert-into :cloudfront-log-files
                    :set
                  :md5 (file-md5 file)
                  :file-name (file-namestring file)
                  :returning :file-id)
                :single!)))
          (commando:with-command-stream (stream ("gzip" "-dc" file))
            (loop
              (let ((line (read-line stream nil)))
                (when (not line)
                  (return))
                (unless (comment-line-p line)
                  (insert-record file-id line))))))))))

(defun process-log-directory (directory)
  (with-connection
    (let ((progress (call-every-nth (lambda () (write-char #\.)) 1000)))
      (do-find-files (file directory "*.gz")
        (funcall progress)
        (process-log-file file)))))
