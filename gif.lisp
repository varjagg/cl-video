;;; Animated GIF processing
;;; One video stream in GIF container

(in-package :cl-video)

(defclass gif-chunk (chunk)
  (;;(pixels :accessor pixels :initarg :pixels)
   (delay :accessor delay :initarg :delay)))

#+nil(defmethod frame ((chunk gif-chunk))
  (pixels chunk))

#+nil(defmethod (setf frame) (pixels (chunk gif-chunk))
  (setf (pixels chunk) pixels))

(defclass gif-stream-record (video-stream-record)
  ((frame-delay :accessor frame-delay :initarg :frame-delay)))

(defmethod pop-chunk-rcursor :before ((rec gif-stream-record))
  (setf (frame-delay rec) (delay (car (rcursor rec)))))

(defclass gif-container (av-container)
  ((number-of-frames :accessor number-of-frames)
   (loopingp :accessor loopingp :initform nil)))

(defmethod decode ((container gif-container))
  (with-open-file (stream (filename container) :direction :input :element-type '(unsigned-byte 8))
    (let* ((data-stream (skippy:read-data-stream stream))
	   (rec (make-instance 'gif-stream-record
			       :container container
			       :frame-delay (/ skippy:*default-delay-time* 100))))
      (with-slots (height width) container
	  (setf height (skippy:height data-stream)
		width (skippy:width data-stream)
		(number-of-frames container) (length (skippy:images data-stream))
 		(loopingp container) (skippy:loopingp data-stream))
	  (initialize-ring rec (number-of-frames container))
	  (loop for image across (skippy:images data-stream)
	     for curpos = (wcursor rec)
	     for frame = (make-array (* height width 3) :element-type '(unsigned-byte 8)) then (copy-array frame) do
	       (setf (car curpos) (make-instance 'gif-chunk :frame frame :delay (/ (skippy:delay-time image) 100)))
	       (loop for y from (skippy:top-position image) below (skippy:height image) do
		    (loop for x from (skippy:left-position image) below (skippy:width image)
		       for pos = (* 3 (+ x (* width y))) do
			 (multiple-value-bind (r g b)
			     (skippy:color-rgb (skippy:color-table-entry (skippy:color-table data-stream) (skippy:pixel-ref image x y)))
			   (setf (aref frame pos) b
				 (aref frame (1+ pos)) g
				 (aref frame (+ pos 2)) r))))
	       (pop (wcursor rec))))
      (push rec (stream-records container))
      (unless (loopingp container)
	(setf (final rec) (car (wcursor rec))))
      (when (player-callback container)
	(funcall (player-callback container) container))
      (bt:release-lock (vacancy-lock (car (wcursor rec)))))))
