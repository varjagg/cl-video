;;;; cl-video.lisp

(in-package #:cl-video)

(setf *print-circle* t)

(eval-when (:compile-toplevel)
  (defconstant +default-fps+ 25)

  (defconstant +avi-dht+
    #(#x01 #xA2 
      #x00 #x00 #x01 #x05 #x01 #x01 #x01 #x01 #x01 #x01 #x00 #x00 #x00 #x00 #x00 
      #x00 #x00 #x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0A #x0B #x01 
      #x00 #x03 #x01 #x01 #x01 #x01 #x01 #x01 #x01 #x01 #x01 #x00 #x00 #x00 #x00 
 
      #x00 #x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0A #x0B #x10 #x00 
      #x02 #x01 #x03 #x03 #x02 #x04 #x03 #x05 #x05 #x04 #x04 #x00 #x00 #x01 #x7D 
      #x01 #x02 #x03 #x00 #x04 #x11 #x05 #x12 #x21 #x31 #x41 #x06 #x13 #x51 #x61 
      #x07 #x22 #x71 #x14 #x32 #x81 #x91 #xA1 #x08 #x23 #x42 #xB1 #xC1 #x15 #x52 
      #xD1 #xF0 #x24 #x33 #x62 #x72 #x82 #x09 #x0A #x16 #x17 #x18 #x19 #x1A #x25 
      #x26 #x27 #x28 #x29 #x2A #x34 #x35 #x36 #x37 #x38 #x39 #x3A #x43 #x44 #x45 
      #x46 #x47 #x48 #x49 #x4A #x53 #x54 #x55 #x56 #x57 #x58 #x59 #x5A #x63 #x64 
 
      #x65 #x66 #x67 #x68 #x69 #x6A #x73 #x74 #x75 #x76 #x77 #x78 #x79 #x7A #x83 
      #x84 #x85 #x86 #x87 #x88 #x89 #x8A #x92 #x93 #x94 #x95 #x96 #x97 #x98 #x99 
      #x9A #xA2 #xA3 #xA4 #xA5 #xA6 #xA7 #xA8 #xA9 #xAA #xB2 #xB3 #xB4 #xB5 #xB6 
      #xB7 #xB8 #xB9 #xBA #xC2 #xC3 #xC4 #xC5 #xC6 #xC7 #xC8 #xC9 #xCA #xD2 #xD3 
      #xD4 #xD5 #xD6 #xD7 #xD8 #xD9 #xDA #xE1 #xE2 #xE3 #xE4 #xE5 #xE6 #xE7 #xE8 
      #xE9 #xEA #xF1 #xF2 #xF3 #xF4 #xF5 #xF6 #xF7 #xF8 #xF9 #xFA #x11 #x00 #x02 
      #x01 #x02 #x04 #x04 #x03 #x04 #x07 #x05 #x04 #x04 #x00 #x01 #x02 #x77 #x00 
 
      #x01 #x02 #x03 #x11 #x04 #x05 #x21 #x31 #x06 #x12 #x41 #x51 #x07 #x61 #x71 
      #x13 #x22 #x32 #x81 #x08 #x14 #x42 #x91 #xA1 #xB1 #xC1 #x09 #x23 #x33 #x52 
      #xF0 #x15 #x62 #x72 #xD1 #x0A #x16 #x24 #x34 #xE1 #x25 #xF1 #x17 #x18 #x19 
      #x1A #x26 #x27 #x28 #x29 #x2A #x35 #x36 #x37 #x38 #x39 #x3A #x43 #x44 #x45 
      #x46 #x47 #x48 #x49 #x4A #x53 #x54 #x55 #x56 #x57 #x58 #x59 #x5A #x63 #x64 
      #x65 #x66 #x67 #x68 #x69 #x6A #x73 #x74 #x75 #x76 #x77 #x78 #x79 #x7A #x82 
      #x83 #x84 #x85 #x86 #x87 #x88 #x89 #x8A #x92 #x93 #x94 #x95 #x96 #x97 #x98 
 
      #x99 #x9A #xA2 #xA3 #xA4 #xA5 #xA6 #xA7 #xA8 #xA9 #xAA #xB2 #xB3 #xB4 #xB5 
      #xB6 #xB7 #xB8 #xB9 #xBA #xC2 #xC3 #xC4 #xC5 #xC6 #xC7 #xC8 #xC9 #xCA #xD2 
      #xD3 #xD4 #xD5 #xD6 #xD7 #xD8 #xD9 #xDA #xE2 #xE3 #xE4 #xE5 #xE6 #xE7 #xE8 
      #xE9 #xEA #xF2 #xF3 #xF4 #xF5 #xF6 #xF7 #xF8 #xF9 #xFA))
  )
(define-condition media-decoder-error (error)
  ())

(define-condition unrecognized-file-format (media-decoder-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignorable condition))
	     (format stream "Unrecognized AVI file format"))))

(define-condition unsupported-avi-file-format (media-decoder-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignorable condition))
	     (format stream "Unsupported AVI file format"))))

(defclass chunk ()
  ((lock :reader vacancy-lock :initform (bt:make-lock))
   (stream-number :accessor stream-number)
   (frame :accessor frame :initarg :frame)))

(defclass video-stream ()
  ((filename :accessor filename :initarg :filename :initform nil)))

(defclass avi-mjpeg-stream (video-stream)
  ((chunk-decoder :accessor chunk-decoder)
   (recommended-buffer-size :accessor recommended-buffer-size :initarg :recommended-buffer-size :initform 16384)
   (chunk-queue :accessor chunk-queue :initarg :chuck-queue :initform (make-list (* 2 +default-fps+)))
   (rcursor :accessor rcursor)
   (wcursor :accessor wcursor)
   (width :accessor width :initarg :width :initform 640)
   (height :accessor height :initarg :height :initform 480)
   (jpeg-descriptor :accessor jpeg-descriptor :initform (cl-jpeg::make-descriptor))
   (padding :accessor padding :initform 1)
   (flags :accessor flags)
   (nstreams :accessor nstreams)
   (fps :accessor fps :initarg :fps :initform +default-fps+)))

(defmethod initialize-instance :after ((s avi-mjpeg-stream) &key)
  (flexi-streams:with-input-from-sequence (is +avi-dht+)
    (jpeg::read-dht (jpeg-descriptor s) is))
  (loop for chunk on (chunk-queue s) do
       (setf (car chunk) (make-instance 'chunk :frame (make-array (* (width s) (height s)) :element-type '(unsigned-byte 8)))))
  (setf (chunk-decoder s) #'(lambda (stream id size)
			      (print id)
			      (cond ((string-equal (subseq id 2) "dc")
				     (bt:with-lock-held ((vacancy-lock (car (wcursor s))))
				       (setf (stream-number (car (wcursor s))) (parse-integer (subseq id 0 1)))
				       (jpeg:decode-stream stream :buffer (frame (car (wcursor s)))
							   :descriptor (jpeg-descriptor s))
				       (pop (wcursor s)))
				     ;; take care of the padding
				       (loop repeat (padding s) do (read-byte s)))
				    (t (read-sequence (make-array size :element-type '(unsigned-byte 8)) stream))))
	(cdr (last (chunk-queue s))) (chunk-queue s)
	(rcursor s) (chunk-queue s)
	(wcursor s) (cdr (chunk-queue s))))

(defgeneric decode (stream)
  (:documentation "Decodes the video stream"))

(defun read-avi-header (avi stream)
  (loop for chunk = (riff:read-riff-chunk stream)
     when (string-equal (riff:riff-chunk-id chunk) "avih") do
       (flexi-streams:with-input-from-sequence (is (riff:riff-chunk-data chunk))
	 (riff:read-u4 is)   ;;dwMicroSecPerFrame
	 (riff:read-u4 is)   ;;dwMaxBytesPerSec
	 (setf (padding avi) (riff:read-u4 is)
	       (flags avi) (riff:read-u4 is))
	 (riff:read-u4 is)   ;;dwTotalFrames
	 (riff:read-u4 is)   ;;dwInitialFrames
	 (setf (nstreams avi) (riff:read-u4 is))
	 (riff:read-u4 is) ;;dwSuggestedBufferSize
	 (setf (width avi) (riff:read-u4 is)
	       (height avi) (riff:read-u4 is))
	 (riff:read-u4 is) (riff:read-u4 is) (riff:read-u4 is) (riff:read-u4 is))
       (return)))

(defmethod decode ((avi avi-mjpeg-stream))
  (with-open-file (stream (filename avi) :direction :input :element-type '(unsigned-byte 8))
    ;; read AVI header first
    (let* ((chunk (riff:read-riff-chunk stream))
	   (id (getf chunk :chunk-id))
	   (fourcc (getf chunk :file-type)))
      (unless (string-equal id "riff")
	(error 'unrecognized-file-format))
      (cond ((string-equal fourcc "avi ") (read-avi-header avi stream))
	    (t (error 'unsupported-avi-file-format))))
    (loop for chunk = (riff:read-riff-chunk stream :chunk-data-reader (chunk-decoder avi))
	 while chunk)))

(defun decode-file (pathname)
  (let ((avi-stream (make-instance 'avi-mjpeg-stream :filename pathname)))
    (decode avi-stream)))

(defun show-file-chunks (pathname)
  (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
    (loop for chunk = (riff:read-riff-chunk stream)
	 while chunk do (format t "~A size ~D~%" (riff:riff-chunk-id chunk) (riff:riff-chunk-data-size chunk)))))
