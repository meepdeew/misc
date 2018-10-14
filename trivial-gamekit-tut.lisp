(ql:quickload :trivial-gamekit)

(defvar *canvas-width*  800)
(defvar *canvas-height* 600)
(defvar *black* (gamekit:vec4 0 0 0 1))
(defvar *origin* (gamekit:vec2 0 0))

(gamekit:defgame hello-gamekit () ()
                 (:viewport-width *canvas-width*)
                 (:viewport-height *canvas-height*)
                 (:viewport-title "Hello Gamekit!"))


(defparameter rwidth  100)
(defparameter rheight 100)

(defvar *current-box-position* (gamekit:vec2 0 0))

(defun real-time-seconds ()
  "Return seconds since certain point of time"
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun update-position (position time)
  ;;;TODO: Why pass in position if it a dynamic variable
  "Update position vector depending on the time supplied"
  (let* ((subsecond (nth-value 1 (truncate time)))
         (angle (* 2 pi subsecond)))
    (setf (gamekit:x position) (+ 350 (* 100 (cos angle)))
          (gamekit:y position) (+ 250 (* 100 (sin angle))))))

(defmethod gamekit:draw ((app hello-gamekit))
  (update-position *current-box-position* (real-time-seconds))
  (gamekit:draw-rect *current-box-position* rheight rwidth :fill-paint *black*))

(gamekit:start 'hello-gamekit)

(gamekit:stop)
(parse-number)
