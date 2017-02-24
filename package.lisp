;;;; package.lisp

(defpackage #:cl-video
  (:use #:cl #:alexandria)
  (:export #:decode-file #:decode #:find-mjpeg-stream-record #:find-pcm-stream-record #:avi-mjpeg-stream
	   #:mjpeg-stream-record #:audio-stream-record #:initialize-ring #:decode-media-stream
	   #:stream-playback-start #:stream-playback-stop #:chunk #:prime-all-streams
	   #:media-decoder-error #:unrecognized-file-format #:unsupported-avi-file-format #:malformed-avi-file-format
	   #:wcursor #:rcursor #:vacancy-lock #:scale #:rate #:filename #:final #:finish
	   #:frame #:pause-lock #:vacancy-lock #:pause #:height #:width #:start #:suggested-buffer-size
	   #:number-of-channels #:block-align #:buffer #:compression-code #:stream-records #:significant-bits-per-sample
	   #:+pcmi-uncompressed+))

