(asdf:defsystem #:cl-video-player
  :description "Video decoder implemented in Common Lisp"
  :author "Eugene Zaikonnikov"
  :license "BSD"
  :depends-on (#:cl-video #:clx #:bordeaux-threads #:static-vectors)
  :serial t
  :components ((:file "player")))
