(in-package :cl-video)

(use-package :kit.sdl2)

(defclass player-window (window)
  ((bitmap :accessor bitmap)))

(defmethod render :after ((window player-window))
  (with-slots (bitmap) window
    ))

(defmethod textinput-event :after ((window player-window) ts text)
  (when (string= "Q" (string-upcase text))
    (close-window window)))

(defmethod keyboard-event :after ((window player-window) state ts repeat-p keysym)
  (let ((scancode (sdl2:scancode keysym)))
    (case scancode
      #+nil(:scancode-left (rewind))
      #+nil(:scancode-right (fwd))
      (:scancode-escape (close-window window)))))
