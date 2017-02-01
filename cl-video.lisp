;;;; cl-video.lisp

(in-package #:cl-video)

(defclass chunk ()
  ((frame :accessor frame :initarg :frame)))

(defclass video-stream ()
  ((filename :accessor filename :initarg :filename :initform nil)))

(defclass avi-mjpeg-stream (video-stream)
  ((chunk-decoder :accessor chunk-decoder)
   (chunk-queue :accessor chunk-queue :initform (make-array 50 :element-type 'chunk :adjustable t))))

(defgeneric decode (stream)
  (:documentation "Decodes the video stream"))


(defmethod make-chunk-hanlder ((av avi-mjpeg-stream))
  #'(lambda (stream id size)
      (declare (ignorable id))
      (let ((chunk-data (make-array size :element-type (stream-element-type stream))))
	(read-sequence chunk-data stream)
	(setf (frame (aref (chunk-queue av) 0)) chunk-data)
	(vector-push-extend (vector-pop (chunk-queue av)) (chunk-queue av)))))
