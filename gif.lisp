;;; Animated GIF processing
;;; One video stream in GIF container

(in-package :cl-video)

(defclass gif-chunk (chunk)
  ((strides :accessor strides :initarg :pixels)
   (pos :reader pos :initarg :pos)
   (delay :reader delay :initarg :delay)
   (span :accessor span :allocation :class :initarg :span)
   (render :accessor render :allocation :class :initarg :render)))

(defmethod frame ((chunk gif-chunk))
  (loop for stride in (strides chunk)
     for index from (pos chunk) by (span chunk) do
       (setf (subseq (render chunk) index) stride))
  (render chunk))

(defclass gif-stream-record (video-stream-record)
  ((frame-delay :accessor frame-delay :initarg :frame-delay)))

(defmethod pop-chunk-rcursor :before ((rec gif-stream-record))
  (setf (frame-delay rec) (delay (car (rcursor rec)))))

(defclass gif-container (av-container)
  ((number-of-frames :accessor number-of-frames)
   (loopingp :accessor loopingp :initform nil)))

(defmethod decode ((container gif-container))
  (with-open-file (stream (filename container) :direction :input :element-type '(unsigned-byte 8))
    (with-slots (height width) container
      (let* ((data-stream (skippy:read-data-stream stream))
	     (rec (make-instance 'gif-stream-record
				 :container container))
	     (buffer (make-array (* height width 3) :element-type '(unsigned-byte 8) :initial-element 0)))
	(setf height (skippy:height data-stream)
	      width (skippy:width data-stream)
	      (number-of-frames container) (length (skippy:images data-stream))
	      (loopingp container) (skippy:loopingp data-stream))
	(initialize-ring rec (number-of-frames container))
	;; initializing class-allocated slots here
	(make-instance 'gif-chunk :span width :render (make-array (* height width 3) :element-type '(unsigned-byte 8) :initial-element 0))
	(loop for image across (skippy:images data-stream)
	   for strides = (loop for y from 0 below (skippy:height image)
			    for rowpos from (skippy:top-position image)
			    for startpos = (* 3 (+ (skippy:left-position image) (* width rowpos))) do
			      (loop for x from 0 below (skippy:width image)
				 for pos from startpos by 3
				 for index = (skippy:pixel-ref image x y) do
				   (unless (eql index (skippy:transparency-index image))
				     (multiple-value-bind (r g b)
					 (skippy:color-rgb (skippy:color-table-entry
							    (or (skippy:color-table image) (skippy:color-table data-stream)) index))
				       (setf (aref buffer pos) b
					     (aref buffer (1+ pos)) g
					     (aref buffer (+ pos 2)) r))))
			      collecting (subseq buffer startpos (+ startpos (* 3 (skippy:width image)))))
	   for curpos = (wcursor rec)
	   do (setf (car curpos) (make-instance 'gif-chunk
					       :pos (+ (skippy:left-position image) (* 3 width (skippy:top-position image)))
					       :strides strides
					       :delay (/ (skippy:delay-time image) 100)))
	     (pop (wcursor rec)))
	(push rec (stream-records container))
	(unless (loopingp container)
	  (setf (final rec) (car (wcursor rec))))
	(when (player-callback container)
	  (funcall (player-callback container) container))
	(bt:release-lock (vacancy-lock (car (wcursor rec))))))))
