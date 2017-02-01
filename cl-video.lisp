;;;; cl-video.lisp

(in-package #:cl-video)

(defclass video-stream ()
  ((filename :accessor filename :initarg :filename :initform nil)))

(defclass avi-mjpeg-stream (video-stream)
  ())

(defgeneric decode (stream)
  (:documentation "Decodes the video stream"))

