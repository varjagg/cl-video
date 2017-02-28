;;; Animated GIF processing
;;; One video stream in GIF container

(in-package :cl-video)

(defclass gif-stream-record (video-stream-record)
  ())

(defmethod shared-initialize :after ((rec gif-stream-record) slots &key &allow-other-keys)
  (declare (ignorable slots))
  (setf (buffer rec) (make-array (suggested-buffer-size rec) :element-type '(unsigned-byte 8))
	(jpeg:descriptor-source-cache (jpeg-descriptor rec)) (buffer rec))
  (initialize-ring rec (max 5 (* 1 (floor (rate rec) (scale rec)))) ;at least 5 chunks to prevent cursor deadlocks
		   (* (height (container rec)) (width (container rec)) 3) 'cl-jpeg::uint8))

(defclass gif-container (av-container)
  ((loopingp :accessor loopingp :initform nil)))

(defmethod decode-media-stream ((rec gif-stream-record) fsize input-stream)
  (let* ((chunk (pop (wcursor rec)))
	 (cur-lock (vacancy-lock chunk))
	 (new-chunk (car (wcursor rec))))
    (bt:acquire-lock (vacancy-lock new-chunk))
    (read-sequence (jpeg:descriptor-source-cache (jpeg-descriptor rec)) input-stream :end fsize)
    (jpeg:decode-stream nil :buffer (frame chunk)
			:descriptor (jpeg-descriptor rec)
			:cached-source-p t)
    (bt:release-lock cur-lock)))

(defmethod decode ((container gif-container))
  (with-open-file (stream (filename container) :direction :input :element-type '(unsigned-byte 8))
    
    (let ((data-stream (skippy:read-data-stream stream))
	  )
      (unless (string-equal id "riff")
	(error 'unrecognized-file-format))
      )
    (when (player-callback container)
      (funcall (player-callback container) container))
    
    (loop for rec in (stream-records container) do 
	 (setf (final rec) (car (wcursor rec)))
	 (bt:release-lock (vacancy-lock (car (wcursor rec)))))))

(defun decode-file (pathname &key player-callback)
  (let ((gif-container (make-instance 'gif-container :filename pathname :player-callback player-callback)))
    (decode gif-container)))
