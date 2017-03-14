;;;; package.lisp

(defpackage #:cl-video
  (:use #:cl #:alexandria)
  (:export #:decode-file #:decode #:find-video-stream-record #:find-pcm-stream-record #:av-container #:avi-mjpeg-container
	   #:mjpeg-stream-record #:audio-stream-record #:initialize-ring #:decode-media-stream #:video-stream-record
	   #:stream-playback-start #:stream-playback-stop #:chunk #:prime-all-streams #:gif-container
	   #:media-decoder-error #:unrecognized-file-format #:unsupported-avi-file-format #:malformed-avi-file-format
	   #:wcursor #:rcursor #:vacancy-lock #:scale #:rate #:filename #:final #:finish #:frame-delay
	   #:frame #:pause-lock #:vacancy-lock #:pause #:height #:width #:start #:suggested-buffer-size
	   #:number-of-channels #:block-align #:buffer #:compression-code #:stream-records #:significant-bits-per-sample
	   #:+pcmi-uncompressed+ #:pop-chunk-rcursor))

