(in-package :cl-video)

(define-constant  +pcmi-uncompressed+ 1)

(define-condition unsupported-wav-file-format (media-decoder-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignorable condition))
	     (format stream "Unsupported WAV file format"))))

(define-condition malformed-wav-file-format (media-decoder-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignorable condition))
	     (format stream "Malformed WAV file format"))))

(defclass wav-stream-record (stream-record)
  ((compression-code :accessor compression-code)
   (number-of-channels :accessor number-of-channels)
   (sample-rate :accessor sample-rate)
   (average-bytes-per-second :accessor average-bytes-per-second)
   (block-align :accessor block-align)
   (significant-bits-per-sample :accessor significant-bits-per-sample)
   (extra-format-bytes :accessor extra-format-bytes)
   (extra-bytes :accessor extra-bytes)))

(defmethod stream-playback-start ((rec wav-stream-record))
  (call-next-method))

(defmethod shared-initialize :after ((rec wav-stream-record) slots &key &allow-other-keys)
  (declare (ignorable slots))
  (setf  (buffer rec) (make-array (suggested-buffer-size rec) :element-type '(unsigned-byte 8))))

(defmethod read-audio-stream-header ((rec wav-stream-record) stream)
  (loop for chunk = (riff:read-riff-chunk stream)
     when (string-equal (riff:riff-chunk-id chunk) "fmt ") do
       (flexi-streams:with-input-from-sequence (is (riff:riff-chunk-data chunk))
	(setf (compression-code rec) (riff:read-u2 is)
	     (number-of-channels rec) (riff:read-u2 is)
	     (sample-rate rec) (riff:read-u4 is)
	     (average-bytes-per-second rec) (riff:read-u4 is)
	     (block-align rec) (riff:read-u2 is)
	     (significant-bits-per-sample rec) (riff:read-u2 is))
	(debug-log (format nil "Audio stream with sample rate ~D at ~D bits per sample" (sample-rate rec) (significant-bits-per-sample rec)))
       (unless (eql (compression-code rec) +pcmi-uncompressed+)
	 (setf (extra-format-bytes rec) (riff:read-u2 is)
	       (extra-bytes rec) (make-array (extra-format-bytes rec) :element-type (stream-element-type is)))
	 (read-sequence (extra-bytes rec) is)))
       (return)))

(defmethod decode-media-stream ((rec wav-stream-record) fsize input-stream)
  (let* ((chunk (pop (wcursor rec)))
	 (cur-lock (vacancy-lock chunk))
	 (new-chunk (car (wcursor rec))))
    (bt:acquire-lock (vacancy-lock new-chunk))
    (read-sequence (frame chunk) input-stream :end fsize)
    (bt:release-lock cur-lock)))

(defclass wav-container (av-container)
  ())

(defmethod initialize-instance :after ((s wav-container) &key &allow-other-keys)
  (setf (chunk-decoder s) #'(lambda (stream id size)
			      (if (member (subseq id 2) '("dc" "wb") :test #'string-equal)
				  (progn (decode-media-stream (elt (stream-records s) (parse-integer (subseq id 0 2))) size stream)
					 (when (plusp (padding s)) (loop repeat (rem size (padding s)) do (read-byte s))))
				  (read-sequence (make-array size :element-type '(unsigned-byte 8)) stream)))))

(defmethod read-wav-stream-info ((wav wav-container) stream)
  (loop for chunk = (riff:read-riff-chunk stream)
     with rec = (make-instance 'wav-stream-record :container wav)
     when (string-equal (riff:riff-chunk-id chunk) "strh") do
       (read-audio-stream-header rec stream)
       (return-from read-wav-stream-info rec))
  (error 'malformed-wav-file-format))

(defmethod read-wav-header ((wav wav-container) stream)
  (loop for chunk = (riff:read-riff-chunk stream)
     when (string-equal (riff:riff-chunk-id chunk) "wavh") do
       (flexi-streams:with-input-from-sequence (is (riff:riff-chunk-data chunk))
	 (riff:read-u4 is)   ;;dwMicroSecPerFrame
	 (riff:read-u4 is)   ;;dwMaxBytesPerSec
	 (setf (padding wav) (riff:read-u4 is)
	       (flags wav) (riff:read-u4 is))
	 (riff:read-u4 is)   ;;dwTotalFrames
	 (riff:read-u4 is)   ;;dwInitialFrames
	 (setf (nstreams wav) (riff:read-u4 is))
	 (riff:read-u4 is) ;;dwSuggestedBufferSize
	 (setf (width wav) (riff:read-u4 is)
	       (height wav) (riff:read-u4 is))
	 (riff:read-u4 is) (riff:read-u4 is) (riff:read-u4 is) (riff:read-u4 is))
       (setf (stream-records wav)
	     (loop repeat (nstreams wav) collecting (read-wav-stream-info wav stream)))
       (return)))

(defmethod find-pcm-stream-record ((container av-container))
  nil)

(defmethod find-pcm-stream-record ((wav wav-container))
  ;; only one stream in wav container
  (car (stream-records wav)))

(defmethod decode ((wav wav-container))
  (with-open-file (stream (filename wav) :direction :input :element-type '(unsigned-byte 8))
    ;; read WAV header first
    (let* ((chunk (riff:read-riff-chunk stream))
	   (id (getf chunk :chunk-id))
	   (fourcc (getf chunk :file-type)))
      (unless (string-equal id "riff")
	(error 'unrecognized-file-format))
      (cond ((string-equal fourcc "wave") (read-wav-header wav stream))
	    (t (error 'unsupported-wav-file-format))))
    (when (player-callback wav)
      (funcall (player-callback wav) wav))
    (loop for chunk = (riff:read-riff-chunk stream :chunk-data-reader (chunk-decoder wav))
       while (and chunk (not (finish wav))))
    (loop for rec in (stream-records wav) do 
	 (setf (final rec) (car (wcursor rec)))
	 (bt:release-lock (vacancy-lock (car (wcursor rec)))))))
