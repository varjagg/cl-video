;;;; package.lisp

(defpackage #:cl-video
  (:use #:cl #:alexandria)
  (:export #:decode-file #:decode #:find-mjpeg-stream-record #:find-pcm-stream-record #:avi-mjpeg-stream
	   #:mjpeg-stream-record #:audio-stream-record
	   #:stream-playback-start #:stream-playback-stop #:chunk
	   #:media-decoder-error #:unrecognized-file-format #:unsupported-avi-file-format #:malformed-avi-file-format
	   #:wcursor #:rcursor #:vacancy-lock #:scale #:rate #:filename #:final #:finish
	   #:frame #:pause-lock #:vacancy-lock #:pause #:height #:width #:start
	   #:number-of-channels #:block-align #:buffer #:compression-code #:stream-records #:+pcmi-uncompressed+))

