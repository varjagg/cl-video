(in-package :cl-video)

;;; we speccialize own class & method to process the audio stream into floats as portaudio wants
;;; but we still want to run decode in another thread to avoid the skips
(defclass portaudio-pcm-stream-record (audio-stream-record)
  ())

(defmethod decode-media-stream ((rec portaudio-pcm-stream-record) fsize input-stream)
  (let* ((chunk (pop (wcursor rec)))
	 (cur-lock (vacancy-lock chunk))
	 (new-chunk (car (wcursor rec))))
    (bt:acquire-lock (vacancy-lock new-chunk))
    (read-sequence (buffer rec) input-stream :end fsize)
    (flexi-streams:with-input-from-sequence (is (buffer rec))
      (let ((sample-size (/ (block-align rec) (number-of-channels rec))))
	(loop for i from 0 below fsize do
	     (setf (aref (frame chunk) i) (if (= sample-size 1)
					    (float (/ (- (read-byte is) 128) 128))
					    (float (/ (let ((u2 (riff:read-u2 is)))
							(if (> u2 32767)
							    (- u2 65536)
							    u2))
						      32768)))))))
    (bt:release-lock cur-lock)))

(defmethod shared-initialize :after ((rec portaudio-pcm-stream-record) slots &key &allow-other-keys)
  (declare (ignorable slots))
  (setf  (buffer rec) (make-array (suggested-buffer-size rec) :element-type '(unsigned-byte 8)))
  (initialize-ring rec 16 (suggested-buffer-size rec) 'float))

(defmethod find-portaudio-stream-record ((container av-container))
  (find-if #'(lambda (x) (and (eql (type-of x) 'portaudio-pcm-stream-record)
			      (eql (compression-code x) +pcmi-uncompressed+))) (stream-records container)))

(defmethod play-audio-stream ((container av-container))
  (let ((audio-rec (find-portaudio-stream-record container))
	astream)
    (when audio-rec
      (bt:make-thread
       #'(lambda ()
	   (portaudio:initialize)
	   (stream-playback-start audio-rec)
	   (setf astream
		 (portaudio:open-default-stream 0 (number-of-channels audio-rec) :float (coerce (sample-rate audio-rec) 'double-float)
						(/ (/ (rate audio-rec) (scale audio-rec)) (number-of-channels audio-rec))))
	   (portaudio:start-stream astream)
	   (unwind-protect
		(loop until (finish container)
		   for cur = (if (pause container) cur (pop (rcursor audio-rec)))
		   for src = (frame cur) do
		   ;; pause synching protocol w/video stream
		     (bt:acquire-lock (pause-lock container))
		   ;; send the audio frame
		     (portaudio:write-stream astream src)
		     (loop while (pause container) do (sleep 0.2))
		     (bt:release-lock (pause-lock container))
		   ;; advance the cursor lock
		     (bt:acquire-lock (vacancy-lock (car (rcursor audio-rec))))
		     (bt:release-lock (vacancy-lock cur))
		     (when (eql cur (final audio-rec))
		       (return))
		     (sleep (frame-delay audio-rec)))
	     (portaudio:close-stream astream)
	     (stream-playback-stop audio-rec)
	     (portaudio:terminate)))
       :name "Audio stream playback"))))
    
(defmethod play-video-stream ((container av-container))
  (bt:make-thread 
   #'(lambda ()
       (with-slots (height width) container
	 (let* ((display (xlib:open-default-display))
		(window (xlib:create-window :parent (xlib:screen-root (xlib:display-default-screen display))
					    :x 0 :y 0 :width width :height height
					    :event-mask '(:exposure :key-press #+nil :resize-redirect)))
		(gc (xlib:create-gcontext :drawable window))
		(pixmap (xlib:create-pixmap :width width :height height :depth 24 :drawable window))
		(pixmap-gc (xlib:create-gcontext :drawable pixmap))
		(buffer (make-array `(,height ,width) :element-type 'xlib:pixel))
		(image (xlib:create-image  :data buffer :depth 24
					   :height height :width width))
		(rec (find-video-stream-record container)))
	   (unwind-protect
		(progn
		  (setf (xlib:wm-name window) (pathname-name (filename container)))
		  (xlib:map-window window)
		  (stream-playback-start rec)
		  (loop with quit = nil until quit
		     for cur = (if (pause container) cur (pop (rcursor rec)))
		     for src = (frame cur) do
		       (loop for i from 0 below height do
			    (loop for j from 0 below width
			       for spos = (* 3 (+ j (* width i))) do
				 (setf (aref buffer i j)
				       (logior (ash (aref src (+ 2 spos)) 16) (ash (aref src (1+ spos)) 8) (aref src spos)))))
		       (xlib:put-image pixmap pixmap-gc image :width width :height height :x 0 :y 0)
		       (xlib:copy-area pixmap gc 0 0 width height window 0 0)
		       (xlib:display-force-output display)
		       (unless (pause container)
			 (bt:acquire-lock (vacancy-lock (car (rcursor rec))))
			 (bt:release-lock (vacancy-lock cur)))
		       (when (eql cur (final rec))
			 (return))
		       (sleep (frame-delay rec))
		       (xlib:event-case (display :timeout 0)
			 (:resize-request ()
					  t)
			 (:exposure ()
				    t)
			 (:key-press (window code)
				     (case (xlib:keysym->character
					    display
					    (xlib:keycode->keysym display code 0))
				       (#\q
					(setf (finish container) t)
					(setf quit t))
				       ((#\Space #\p)
					(setf (pause container) (not (pause container)))
					(bt:acquire-lock (pause-lock container))
					(bt:release-lock (pause-lock container))))
				     t))))
	     (stream-playback-stop rec)
	     (xlib:free-pixmap pixmap)
	     (xlib:free-gcontext gc)
	     (xlib:close-display display)))))
   :name "Video stream playback"))

(defun decode-file (pathname &key player-callback)
  (let ((container (make-instance (cond  ((string-equal "avi" (pathname-type pathname)) 'avi-mjpeg-container)
					 ((string-equal "gif" (pathname-type pathname)) 'gif-container)
					 (t (error 'unrecognized-file-format))) :filename pathname :player-callback player-callback)))
    (decode container)))

(defun play (pathname)
  (decode-file pathname :player-callback
	       #'(lambda (video)
		   ;;has to use our specific decode for audio
		   (let ((a (find-pcm-stream-record video)))
		     (when a (change-class a 'portaudio-pcm-stream-record)))
		   (prime-all-streams video)
		   (play-audio-stream video) (play-video-stream video))))
