;;;; utils.lisp

(in-package #:cloudfront-log-processor)

(defun split-tabs (string)
  (ppcre:split "\\\t" string))

(defun file-md5 (file)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-file 'ironclad:md5 file)))

(defun file-aws-etag (file)
  (format nil "~S" (file-md5 file)))

(defun file-processed-p (file)
  (let ((md5 (file-md5 file)))
    (postmodern:query (:select :md5
                        :from :cloudfront-log-files
                        :where (:= md5 :md5)) )))

(defun comment-line-p (line)
  (and (plusp (length line))
       (eql (char line 0) #\#)))

(defparameter *log-record-fields*
  '(:date :time :x-edge-location
    :sc-bytes :c-ip
    :cs-method
    :cs-host
    :cs-uri-stem
    :sc-status :cs-referer :cs-user-agent
    :cs-uri-query :cs-cookie
    :x-edge-result-type
    :x-edge-request-id
    :x-host-header
    :cs-protocol
    :cs-bytes
    :time-taken))

(defun record-plist (line)
  (mapcan 'list *log-record-fields* (split-tabs line)))

(defmacro insert-plist (table plist &rest keys)
  (flet ((binding (keyword)
           (list (copy-symbol keyword) :null)))
    (let ((bindings (mapcar #'binding keys)))
      `(destructuring-bind (&key ,@bindings)
           ,plist
         (postmodern:query
          (:insert-into ,table
              :set
            ,@ (loop for key in keys
                     for binding in bindings
                     collect key collect (first binding))))))))

(defun insert-record (file-id line)
  (insert-plist :cloudfront-log-records
                (list* :file-id file-id (record-plist line))
                :file-id
                :date :time :x-edge-location
                :sc-bytes :c-ip
                :cs-method
                :cs-host
                :cs-uri-stem
                :sc-status :cs-referer :cs-user-agent
                :cs-uri-query :cs-cookie
                :x-edge-result-type
                :x-edge-request-id
                :x-host-header
                :cs-protocol
                :cs-bytes
                :time-taken))

(defun call-for-find-files (fun directory pattern)
  (commando:with-command-stream  (stream ("find" (pathname directory)
                                                 "-type" "f"
                                                 "-name" pattern))
    (loop
      (let ((line (read-line stream nil)))
        (unless line
          (return))
        (funcall fun line)))))

(defmacro do-find-files ((file directory pattern) &body body)
  `(call-for-find-files (lambda (,file)
                          ,@body)
                        ,directory
                        ,pattern))

(defun call-every-nth (fun n)
  (let ((counter 0))
    (lambda ()
      (when (<= n (incf counter))
        (funcall fun)
        (setf counter 0)))))
