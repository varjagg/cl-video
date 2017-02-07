;;;; package.lisp

(defpackage #:cl-video
  (:use #:cl)
  (:export #:decode-file #:decode #:find-mjpeg-stream-record #:avi-mjpeg-stream
	   #:mjpeg-stream-record #:audio-stream-record
	   #:stream-playback-start #:stream-playback-stop #:chunk
	   #:media-decoder-error #:unrecognized-file-format #:unsupported-avi-file-format #:malformed-avi-file-format
	   #:rcursor #:vacancy-lock #:scale #:rate #:filename #:final))

