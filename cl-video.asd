;;;; cl-video.asd

(asdf:defsystem #:cl-video
  :description "Video decoder implemented in Common Lisp"
  :author "Eugene Zaikonnikov"
  :license "BSD"
  :version "1.3"
  :depends-on (#:bordeaux-threads #:flexi-streams)
  :serial t
  :components ((:file "package")
               (:file "cl-video")))

(asdf:defsystem #:cl-video-avi
  :description "AVI MJPEG/PCM module of CL-VIDEO"
  :author "Eugene Zaikonnikov"
  :license "BSD"
  :depends-on (#:alexandria #:cl-riff #:cl-jpeg #:cl-video)
  :serial t
  :components ((:file "avi")))

(asdf:defsystem #:cl-video-gif
  :description "Animated GIF module of CL-VIDEO"
  :author "Eugene Zaikonnikov"
  :license "BSD"
  :depends-on (#:cl-video #:skippy)
  :serial t
  :components ((:file "gif")))
