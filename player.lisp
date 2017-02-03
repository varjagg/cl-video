(in-package :cl-video)

(use-package :kit.sdl2)

(defclass player-window (window)
  ((avi :accessor avi :initarg :avi)
   (bitmap :accessor bitmap)))

(defmethod initialize-instance :after ((w player-window) &key &allow-other-keys)
  (setf (window-size w) (cons 640 480))
  (setf (window-title w) "cl-video-player")
  (setf (idle-render w) t))

(defmethod render :after ((window player-window))
  (with-slots (bitmap) window
    (create-rgb-surface-from )))

(defmethod textinput-event :after ((window player-window) ts text)
  (when (string= "Q" (string-upcase text))
    (close-window window)))

(defmethod keyboard-event :after ((window player-window) state ts repeat-p keysym)
  (let ((scancode (sdl2:scancode keysym)))
    (case scancode
      #+nil(:scancode-left (rewind))
      #+nil(:scancode-right (fwd))
      (:scancode-escape (close-window window)))))
