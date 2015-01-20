;;;; command-line.lisp

(in-package #:cloudfront-log-processor)

(defun command-line-debugger (condition previous-hook)
  "The function called when there are errors in the command-line
  application."
  (declare (ignore previous-hook))
  (format *error-output* "~&Fatal ~A: ~%  ~A~%"
          (type-of condition)
          condition)
  (sb-ext:exit :code 1))

(define-flag *bucket*
    :selector "bucket"
    :type string
    :default-value "")

(define-flag *prefix*
    :selector "prefix"
    :type string
    :default-value "")

(define-flag *credentials-file*
    :selector "credentials-file"
    :type string
    :default-value "credentials.txt")

(defmacro check-not-empty-p (var)
  `(when (equal ,var "")
     (error "~S must not be empty string" ',var)))

(defun main (argv)
  (setf sb-ext:*invoke-debugger-hook* 'command-line-debugger)
  (parse-command-line argv)
  (check-not-empty-p *bucket*)
  (check-not-empty-p *prefix*)
  (unless (probe-file *credentials-file*)
    (error "Credentials file ~S not found; ~
            use --credentials-file to specify a valid file"
           *credentials-file*))
  (with-logging
    (let ((zs3:*credentials* (zs3:file-credentials *credentials-file*)))
      (process-cloudfront-logs *bucket* *prefix*))))
