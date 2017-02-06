(asdf:defsystem #:cl-video-player
  :description "Video decoder implemented in Common Lisp"
  :author "Eugene Zaikonnikov"
  :license "BSD"
  :depends-on (#:cl-video #:sdl2kit #:bordeaux-threads #:static-vectors)
  :serial t
  :components ((:file "player")))
