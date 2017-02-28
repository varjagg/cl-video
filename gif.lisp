;;; Animated GIF processing
;;; One video stream in GIF container

(in-package :cl-video)

(defclass gif-stream-record (video-stream-record)
  ())

(defclass gif-container (av-container)
  ((number-of-frames :accessor number-of-frames)
   (loopingp :accessor loopingp :initform nil)))

(defmethod decode ((container gif-container))
  (with-open-file (stream (filename container) :direction :input :element-type '(unsigned-byte 8))
    (let* ((data-stream (skippy:read-data-stream stream))
	   (rec (make-instance 'gif-stream-record
			       :container container
			       :frame-delay (/ (skippy:delay-time (aref (skippy:images data-stream) 0)) 100))))
      (with-slots (height width) container
	  (setf height (skippy:height data-stream)
		width (skippy:width data-stream)
		(number-of-frames container) (length (skippy:images data-stream))
 		(loopingp container) (skippy:loopingp data-stream))
	  (initialize-ring rec (number-of-frames container)
			   (* height width 3) '(unsigned-byte 8))
	  (loop for image across (skippy:images data-stream)
	     for cur = (pop (wcursor rec))
	     for frame = (frame cur) do
	       (debug-log (format nil "~D ~D~%" (skippy:height image) (skippy:width image)))
	       (loop for y from 0 below height do
		    (loop for x from 0 below width
		       for pos = (* 3 (+ x (* width y))) do
			 (multiple-value-bind (r g b)
			     (skippy:color-rgb (skippy:color-table-entry (skippy:color-table data-stream) (skippy:pixel-ref image x y)))
			   (setf (aref frame pos) b
				 (aref frame (1+ pos)) g
				 (aref frame (+ pos 2)) r))))))
      (unless (loopingp container)
	(setf (final rec) (car (wcursor rec))))
      (when (player-callback container)
	(funcall (player-callback container) container))
      (bt:release-lock (vacancy-lock (car (wcursor rec)))))))
