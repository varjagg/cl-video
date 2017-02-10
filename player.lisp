(in-package :cl-video)

(defmethod play-audio-stream ((avi avi-mjpeg-stream))
  (let ((audio-rec (find-pcm-stream-record avi))
	astream)
    (when audio-rec
      (bt:make-thread
       #'(lambda ()
	   (portaudio:initialize)
	   (stream-playback-start audio-rec)
	   (setf astream
		 (portaudio:open-default-stream 0 (number-of-channels audio-rec) :float (coerce (sample-rate audio-rec) 'double-float)
						(/ (rate audio-rec) (scale audio-rec))))
	   (sleep (* (start audio-rec) (/ (scale audio-rec) (rate audio-rec))))
	   (portaudio:start-stream astream)
	   (unwind-protect
		(loop for cur = (if (pause avi) cur (pop (rcursor audio-rec)))
		   for src = (frame cur)
		   until (finish avi) do
		   ;; pause synching protocol w/video stream
		     (bt:acquire-lock (pause-lock avi))
		   ;; send the audio frame
		     (portaudio:write-stream astream src)
		     (loop while (pause avi) do (sleep 0.2))
		     (bt:release-lock (pause-lock avi))
		   ;; advance the cursor lock
		     (bt:acquire-lock (vacancy-lock (car (rcursor audio-rec))))
		     (bt:release-lock (vacancy-lock cur))
		     (when (eql cur (final audio-rec))
		       (return))
		     (sleep (/ (scale audio-rec) (rate audio-rec))))
	     (portaudio:close-stream astream)
	     (stream-playback-stop audio-rec)
	     (portaudio:terminate)))))))
    
(defmethod play-video-stream ((avi avi-mjpeg-stream))
  (bt:make-thread 
   #'(lambda ()
       (with-slots (height width) avi
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
		(rec (find-mjpeg-stream-record avi)))
	   (sleep (* (start rec) (/ (scale rec) (rate rec)))) ;stream delay, if any
	   (unwind-protect
		(progn
		  (setf (xlib:wm-name window) (pathname-name (filename avi)))
		  (xlib:map-window window)
		  (stream-playback-start rec)
		  (loop for cur = (if (pause avi) cur (pop (rcursor rec)))
		     for src = (frame cur)
		     with quit = nil until quit do
		       (loop for i from 0 below height do
			    (loop for j from 0 below width
			       for spos = (* 3 (+ j (* width i))) do
				 (setf (aref buffer i j)
				       (logior (ash (aref src (+ 2 spos)) 16) (ash (aref src (1+ spos)) 8) (aref src spos)))))
		       (xlib:put-image pixmap pixmap-gc image :width width :height height :x 0 :y 0)
		       (xlib:copy-area pixmap gc 0 0 width height window 0 0)
		       (xlib:display-force-output display)
		       (unless (pause avi)
			 (bt:acquire-lock (vacancy-lock (car (rcursor rec))))
			 (bt:release-lock (vacancy-lock cur)))
		       (when (eql cur (final rec))
			 (return))
		       (sleep (/ (scale rec) (rate rec)))
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
					(setf (finish avi) t)
					(setf quit t))
				       ((#\Space #\p)
					(setf (pause avi) (not (pause avi)))
					(bt:acquire-lock (pause-lock avi))
					(bt:release-lock (pause-lock avi))))
				     t))))
	   (stream-playback-stop rec)
	   (xlib:free-pixmap pixmap)
	   (xlib:free-gcontext gc)
	   (xlib:close-display display)))))))

(defun play (pathname)
  (decode-file pathname :player-callback #'(lambda (avi) (play-audio-stream avi) (play-video-stream avi))))
