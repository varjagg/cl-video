(in-package :cl-video)
 
(defmethod play-stream ((avi avi-mjpeg-stream))
  (bt:make-thread 
   #'(lambda ()
       (with-slots (height width stream-records) avi
	 (let* ((display (xlib:open-default-display))
		(window (xlib:create-window :parent (xlib:screen-root (xlib:display-default-screen display))
					    :x 0 :y 0 :width width :height height
					    :event-mask '(:exposure :key-press)))
		(gc (xlib:create-gcontext :drawable window))
		(pixmap (xlib:create-pixmap :width width :height height :depth 24 :drawable window))
		(pixmap-gc (xlib:create-gcontext :drawable pixmap))
		(buffer (make-array `(,width ,height) :element-type 'xlib:pixel))
		(image (xlib:create-image  :data buffer :depth 24
					   :height height :width width))
		(rec (find-if #'(lambda (x) (eql (type-of x) 'mjpeg-stream-record)) stream-records)))
	   (unwind-protect
		(progn
		  (xlib:event-case (display :discard-p t)
		    (exposure ()
			      (loop for i from 0 below height
				 with src = (frame (pop (rcursor rec))) do
				   (loop for j from 0 below width
				      for spos = (* 3 (+ j (* width i))) do
					(setf (aref buffer j i)
					      (logior (ash (aref src (+ 2 spos)) 16) (ash (aref src (1+ spos)) 8) (aref src spos)))))
			      (sleep (/ (scale rec) (rate rec)))
			      (xlib:put-image pixmap pixmap-gc image :width width :height height :x 0 :y 0)
			      (xlib:copy-area pixmap gc 0 0 width height window 0 0)
			      (xlib:display-force-output display)
			      nil)
		    (key-press ()
			       t))))
	   (xlib:free-pixmap pixmap)
	   (xlib:free-gcontext gc)
	   (xlib:close-display display))))))

(defun play (pathname)
  (decode-file pathname :player-callback #'play-stream))
