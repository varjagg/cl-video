;;;; cl-video.lisp

(in-package #:cl-video)

(setf *print-circle* t)

(defclass chunk ()
  ((frame :accessor frame :initarg :frame)))

(defclass video-stream ()
  ((filename :accessor filename :initarg :filename :initform nil)))

(defclass avi-mjpeg-stream (video-stream)
  ((chunk-decoder :accessor chunk-decoder)
   (chunk-queue :accessor chunk-queue :initform (let ((l (make-list 50)))
						  (setf (cdr (last l)) l)
						  l))))

(defgeneric decode (stream)
  (:documentation "Decodes the video stream"))

(defmethod make-chunk-hanlder ((av avi-mjpeg-stream))
  #'(lambda (stream id size)
      (declare (ignorable id))
      (let ((chunk-data (make-array size :element-type (stream-element-type stream))))
	(read-sequence chunk-data stream)
	(setf (frame (aref (chunk-queue av) 0)) chunk-data)
	(vector-push-extend (vector-pop (chunk-queue av)) (chunk-queue av)))))

