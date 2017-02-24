;;;; cl-video.asd

(asdf:defsystem #:cl-video
  :description "Video decoder implemented in Common Lisp"
  :author "Eugene Zaikonnikov"
  :license "BSD"
  :version "1.3"
  :depends-on (#:alexandria #:cl-riff #:cl-jpeg #:bordeaux-threads #:flexi-streams)
  :serial t
  :components ((:file "package")
               (:file "cl-video")))
