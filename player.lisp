(in-package :cl-video)
#|
(use-package :kit.sdl2)

(defclass player-window (window)
  ((avi :accessor avi :initarg :avi)
   (bitmap :accessor bitmap)))

(defmethod initialize-instance :after ((w player-window) &key &allow-other-keys)
  (setf (window-size w) (cons 640 480))
  (setf (window-title w) "cl-video-player")
  (setf (bitmap w) (static-vectors:make-static-vector (* 640 480 3) :initial-element 0)))

(defmethod render :after ((window player-window))
  (with-slots (bitmap) window
    (sdl2:blit-surface (sdl2:create-rgb-surface-from (static-vectors:static-vector-pointer bitmap) 640 480 24 (* 640 3))
		       nil
		       (sdl2:get-window-surface (sdl-window window))
		       (sdl2:make-rect 0 0 640 480))
    (sdl2:fill-rect (sdl2:get-window-surface (sdl-window window)) (sdl2:make-rect 0 0 640 480) #xffffffff)
    (sdl2:update-window (sdl-window window))))

(defmethod textinput-event :after ((window player-window) ts text)
  (when (string= "Q" (string-upcase text))
    (close-window window)))

(defmethod keyboard-event :after ((window player-window) state ts repeat-p keysym)
  (let ((scancode (sdl2:scancode keysym)))
    (case scancode
      #+nil(:scancode-left (rewind))
      #+nil(:scancode-right (fwd))
      (:scancode-escape (close-window window)))))

(defmethod close-window ((w player-window))
  (static-vectors:free-static-vector (bitmap w))
  (call-next-method))

(defun play ()
  (sdl2.kit:init)
  (make-instance 'player-window))

|#

(use-package :xlib)
 
(defmethod play-stream ((avi avi-mjpeg-stream))
  (bt:make-thread 
   #'(lambda ()
       (with-slots (height width stream-records) avi
	 (let* ((display (open-default-display))
		(window (create-window :parent (screen-root (display-default-screen display))
				       :x 0
				       :y 0
				       :width width
				       :height height
				       :event-mask '(:exposure :key-press)))
		(gc (create-gcontext :drawable window))
		(pixmap (create-pixmap :width width :height height :depth 24 :drawable window))
		(pixmap-gc (create-gcontext :drawable pixmap))
		(buffer (make-array `(,width ,height) :element-type 'xlib:pixel))
		(image (create-image  :data buffer :depth 24
				      :height height :width width))
		(rec (find-if #'(lambda (x) (eql (type-of x) 'mjpeg-stream-record)) stream-records)))
	   (unwind-protect
		(setf (wm-name window) (pathname-name (filename avi)))
	     (map-window window)
	     (event-case (display :discard-p t)
	       (exposure ()
			 (loop for i from 0 below height
			    with src = (frame (pop (rcursor rec))) do
			      (loop for j from 0 below width
				 for spos = (* 3 (+ j (* width i))) do
				      (setf (aref buffer j i)
					    (logand (ash (aref src (+ 2 spos)) 16) (ash (aref src (1+ spos)) 8) (aref src spos)))))
			 (sleep (/ (scale rec) (rate rec)))
			 (put-image pixmap pixmap-gc image :width width :height height :x 0 :y 0)
			 (copy-area pixmap gc 0 0 width height window 0 0)
			 (display-force-output display)
			 nil #| continue receiving events |#)
	       (key-press ()
			  t #| non-nil result signals event-case to exit |#)))
	 (free-pixmap pixmap)
	 (free-gcontext gc)
	 (close-display display))))))

(defun play (pathname)
  (decode-file pathname :player-callback #'play-stream))

