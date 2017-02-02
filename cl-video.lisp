;;;; cl-video.lisp

(in-package #:cl-video)

(setf *print-circle* t)

(defclass chunk ()
  ((frame :accessor frame :initarg :frame)))

(defclass video-stream ()
  ((filename :accessor filename :initarg :filename :initform nil)))

(defclass avi-mjpeg-stream (video-stream)
  ((chunk-decoder :accessor chunk-decoder)
   (recommended-buffer-size :accessor recommended-buffer-size :initarg :recommended-buffer-size :initform 16384)
   (chunk-queue :accessor chunk-queue :initform (make-list 50))
   (rcursor :accessor rcursor)
   (wcursor :accessor wcursor)))

(defmethod initialize-instance :after ((s avi-mjpeg-stream) &key)
  (loop for chunk on (chunk-queue s) do
       (setf (car chunk) (make-instance 'chunk :frame (make-array (recommended-buffer-size s) :element-type '(unsigned-byte 8)))))
  (setf (chunk-decoder s) #'(lambda (stream id size)
			      (declare (ignorable id))
			      (when (> size (length (frame (wcursor av))))
				(setf (frame (wcursor av)) (make-array size :element-type '(unsigned-byte 8))))
			      (read-sequence (frame (wcursor av)) stream))
	(cdr (last (chunk-queue s))) (chunk-queue s)
	(rcursor s) (chunk-queue s)
	(wcursor s) (cdr (chunk-queue s))))

(defgeneric decode (stream)
  (:documentation "Decodes the video stream"))

(defmethod decode ((avi avi-mjpeg-stream))
  (with-open-file (stream (filename avi) :direction :input :element-type '(unsigned-byte 8))
    (riff:read-riff-chunk stream :chunk-data-reader (chunk-decoder avi))))

(defun decode-file (pathname)
    (let ((avi-stream (make-instance 'avi-mjpeg-stream :filename pathname)))
      (decode avi-stream)))
