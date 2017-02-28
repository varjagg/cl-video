(asdf:defsystem #:cl-video-avi
  :description "AVI MJPEG/PCM module of CL-VIDEO"
  :author "Eugene Zaikonnikov"
  :license "BSD"
  :depends-on (#:alexandria #:cl-riff #:cl-jpeg #:cl-video)
  :serial t
  :components ((:file "avi")))

