(in-package :cl-video)

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
