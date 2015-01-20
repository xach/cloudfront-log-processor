;;;; cloudfront-log-processor.asd

(asdf:defsystem #:cloudfront-log-processor
  :description "Import cloudfront logfiles into Postgres."
  :author "Zach Beane <xach@xach.com>"
  :license "MIT"
  :depends-on (#:zs3
               #:commando
               #:postmodern
               #:cl-ppcre
               #:com.google.flag)
  :serial t
  :components ((:file "package")
               (:file "database")
               (:file "utils")
               (:file "logging")
               (:file "cloudfront-log-processor")
               (:file "import")
               (:file "command-line")))

