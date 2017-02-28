;;;; Video decoder implementation
;;;; Supports MJPEG in AVI container
;;;; (c) 2017 Eugene Zaikonnikov <eugene@fucall.org>

(in-package #:cl-video)

(setf *print-circle* t)

(defvar *debug* nil)

(defun debug-log (string)
  (when *debug*
    (format t "~A~%" string)
    (finish-output)))

(define-condition media-decoder-error (error)
  ())

(define-condition unrecognized-file-format (media-decoder-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignorable condition))
	     (format stream "Unrecognized file format"))))

(defclass chunk ()
  ((lock :reader vacancy-lock :initform (bt:make-lock "vacancy"))
   (frame :accessor frame :initarg :frame)))

(defclass stream-record ()
  ((chunk-queue :accessor chunk-queue)
   (rcursor :accessor rcursor)
   (wcursor :accessor wcursor)
   (final :accessor final :initform nil)
   (buffer :accessor buffer :type '(unsigned-byte 8))
   (container :accessor container :initarg :container)))

(defmethod initialize-ring ((rec stream-record) ring-length frame-size element-type)
  (setf (chunk-queue rec) (make-list ring-length))
  (loop for chunk on (chunk-queue rec) do
       (setf (car chunk) (make-instance 'chunk :frame (make-array frame-size :element-type element-type))))
  (setf (cdr (last (chunk-queue rec))) (chunk-queue rec)
	(rcursor rec) (chunk-queue rec)
	(wcursor rec) (cdr (chunk-queue rec))))

(defmethod stream-playback-start ((rec stream-record))
  (bt:acquire-lock (vacancy-lock (car (rcursor rec)))))

(defmethod stream-playback-stop ((rec stream-record))
  (bt:release-lock (vacancy-lock (car (rcursor rec)))))

(defclass video-stream-record (stream-record)
  (frame-delay :accessor frame-delay :initarg :frame-delay))

(defclass av-container ()
  ((filename :accessor filename :initarg :filename :initform nil)
   (player-callback :accessor player-callback :initarg :player-callback :initform nil) ;;called once all headers are processed
   (width :accessor width :initarg :width :initform 640)
   (height :accessor height :initarg :height :initform 480)
   (stream-records :accessor stream-records)
   (finish :accessor finish :initform nil)
   (pause :accessor pause :initform nil)
   (pause-lock :accessor pause-lock :initform (bt:make-lock "pause"))))

(defgeneric decode-media-stream (record fsize input-stream))

(defmethod decode-media-stream ((rec stream-record) fsize input-stream)
  (read-sequence (frame (ar (wcursor rec))) input-stream :end fsize))

(defgeneric decode (stream)
  (:documentation "Decodes the video stream"))

(defmethod prime-all-streams ((c av-container))
  ;; the playback shouldn't start before 1st frame is decoded
  (loop for rec in (stream-records c) do
       (bt:acquire-lock (vacancy-lock (car (wcursor rec))))))

(defmethod find-video-stream-record ((c av-container))
  (find-if #'(lambda (x) (typep x 'video-stream-record)) (stream-records c)))
