;;;; cl-video.asd

(asdf:defsystem #:cl-video
  :description "Video decoder implemented in Common Lisp"
  :author "Eugene Zaikonnikov"
  :license "BSD"
  :depends-on (#:cl-riff #:cl-jpeg #:bordeaux-threads)
  :serial t
  :components ((:file "package")
               (:file "cl-video")))
