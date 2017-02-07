(in-package :cl-video)
 
(defmethod play-stream ((avi avi-mjpeg-stream))
  (bt:make-thread 
   #'(lambda ()
       (with-slots (height width) avi
	 (let* ((display (xlib:open-default-display))
		(window (xlib:create-window :parent (xlib:screen-root (xlib:display-default-screen display))
					    :x 0 :y 0 :width width :height height
					    :event-mask '(:exposure :key-press)))
		(gc (xlib:create-gcontext :drawable window))
		(pixmap (xlib:create-pixmap :width width :height height :depth 24 :drawable window))
		(pixmap-gc (xlib:create-gcontext :drawable pixmap))
		(buffer (make-array `(,height ,width) :element-type 'xlib:pixel))
		(image (xlib:create-image  :data buffer :depth 24
					   :height height :width width))
		(rec (find-mjpeg-stream-record avi)))
	   (unwind-protect
		(progn
		  (setf (xlib:wm-name window) (pathname-name (filename avi)))
		  (xlib:map-window window)
		  (stream-playback-start rec)
		  (loop for cur = (pop (rcursor rec))
		     for src = (frame cur) do
		       (loop for i from 0 below height do
			    (loop for j from 0 below width
			       for spos = (* 3 (+ j (* width i))) do
				 (setf (aref buffer i j)
				       (logior (ash (aref src (+ 2 spos)) 16) (ash (aref src (1+ spos)) 8) (aref src spos)))))
		       (xlib:put-image pixmap pixmap-gc image :width width :height height :x 0 :y 0)
		       (xlib:copy-area pixmap gc 0 0 width height window 0 0)
		       (xlib:display-force-output display)
		       (bt:acquire-lock (vacancy-lock (car (rcursor rec))))
		       (bt:release-lock (vacancy-lock cur))
		       (when (eql cur (final rec))
			 (return))
		       (sleep (/ (scale rec) (rate rec)))
		       #+nil(xlib:event-case (display :discard-p t)
			 (resize-request ()
					 t)
			 (key-press ()
				    (return)
				    t))))
	     (stream-playback-stop rec)
	     (xlib:free-pixmap pixmap)
	     (xlib:free-gcontext gc)
	     (xlib:close-display display)))))))

(defun play (pathname)
  (decode-file pathname :player-callback #'play-stream))
