(in-package :b)

;;;; Parameters ---------------------------------------------------------------
(defvar *running* t)

(defparameter *screen-width* 64)
(defparameter *screen-height* 48)
(defparameter *cell-size* 8)
(defparameter *assets-directory*
  (merge-pathnames #p"assets/" (deploy:data-directory)))


(defparameter *player-velocity* 13.5) ; world cells per second
(defparameter *player-breath-time* 0.2)
(defparameter *player-breath-distance* 0.2)


(defparameter *terrain-bottom-offset* 10000.0)
(defparameter *terrain-noise-scale* 0.2)
(defparameter *terrain-max-height-top* 20.0)
(defparameter *terrain-max-height-bottom* 10.0)
(defparameter *terrain-seed* 0.0)

(defvar *player* nil)
(defvar *inputs* (make-hash-table))
(defvar *camera-x* 0)
(defvar *camera-y* 0)


;;;; Config -------------------------------------------------------------------
(defun asset-path (filename)
  (-<> filename
    (merge-pathnames (pathname <>) *assets-directory*)
    (namestring <>)))

(defun config-fonts ()
  (blt:set "font: ~A, size=~Dx~:*~D, spacing=2x2;"
           (asset-path "ProggySquare/ProggySquare.ttf")
           (* 2 *cell-size*))
  (blt:set "tile font: ~A, size=~Dx~:*~D, spacing=2x2;"
           (asset-path "ProggySquare/ProggySquare.ttf")
           (* 2 *cell-size*))
  (blt:set "text font: ~A, size=~Dx~D, spacing=1x2;"
           (asset-path "UbuntuMono/UbuntuMono-R.ttf")
           (* 2 *cell-size*)
           *cell-size*))

(defun config ()
  (assert (evenp *cell-size*))
  (blt:set (format nil "window.size = ~Dx~D"
                   (* 2 *screen-width*)
                   (* 2 *screen-height*)))
  (blt:set "window.title = B@TTY")
  (blt:set "window.cellsize = ~Dx~:*~D" *cell-size*)
  (blt:set "window.resizeable = false")
  (blt:set "output.vsync = true")
  (blt:set "input.filter = keyboard+")
  (config-fonts))


;;;; Terrain ------------------------------------------------------------------
(defun noise-value-to-terrain-height (v bottom?)
  (-<> v
    (+ <> 1)
    (/ <> 2)
    (* <> (if bottom? *terrain-max-height-bottom* *terrain-max-height-top*))))

(defun terrain-height (x bottom?)
  (1+ (noise-value-to-terrain-height
        (black-tie:simplex-noise-1d
          (+ *terrain-seed*
             (* x *terrain-noise-scale*)
             (if bottom? *terrain-bottom-offset* 0.0)))
        bottom?)))


;;;; Aspects ------------------------------------------------------------------
(define-aspect loc
  (x :initform 0.0 :type single-float)
  (y :initform 0.0 :type single-float))

(define-aspect moveable
  (vx :initform 0.0 :type single-float)
  (vy :initform 0.0 :type single-float))

(define-aspect renderable
  (glyph :initform #\? :type character)
  (color :initform (blt:white)))


;;;; Player -------------------------------------------------------------------
(define-entity player (loc renderable moveable)
  (breath-time :accessor player/breath :initform 0.0))

(defun make-player ()
  (create-entity 'player
    :renderable/glyph #\@
    :loc/x (/ *screen-width* 2.0)
    :loc/y (/ *screen-height* 2.0)))

(defun tick-player-input (player)
  (setf
    (moveable/vy player) (cond
                           ((gethash :up *inputs*) (- *player-velocity*))
                           ((gethash :down *inputs*) *player-velocity*)
                           (t 0.0))
    (moveable/vx player) (cond
                           ((gethash :left *inputs*) (- *player-velocity*))
                           ((gethash :right *inputs*) *player-velocity*)
                           (t 0.0))))

(defun tick-player-position (player delta-time)
  (incf (loc/x player) (* delta-time (moveable/vx player)))
  (incf (loc/y player) (* delta-time (moveable/vy player))))

(defun tick-player-breath (player delta-time)
  (let* ((old (player/breath player))
         (new (mod (+ old delta-time) (* 2 *player-breath-time*))))
    (setf (player/breath player) new)
    (cond
      ((< new old)
       (decf (loc/y player) *player-breath-distance*))
      ((< old *player-breath-time* new)
       (incf (loc/y player) *player-breath-distance*)))))

(defun tick-player (player delta-time)
  (tick-player-input player)
  (tick-player-position player delta-time)
  (tick-player-breath player delta-time))


;;;; Game Logic ---------------------------------------------------------------
(defun tick-camera ()
  (when (>= (- (loc/x *player*) *camera-x*)
            (* 0.90 *screen-width*))
    (incf *camera-x* 10)))

(defun tick (delta-time)
  (tick-player *player* delta-time)
  (tick-camera))


;;;; UI -----------------------------------------------------------------------
(defun screen-coords (x y)
  "Translate world coordinates into screen coordinates.

  Returns four values:

  * The main screen coordinates
  * The pixel offsets into that tile

  "
  (nest (multiple-value-bind (sx xr) (truncate (* 2 (- x *camera-x*))))
        (multiple-value-bind (sy yr) (truncate (* 2 (- y *camera-y*))))
        (values sx sy
                (truncate (* *cell-size* xr))
                (truncate (* *cell-size* yr)))))


(defun blit-player (entity)
  (setf (blt:layer) 2)
  (multiple-value-bind (x y dx dy)
      (screen-coords (loc/x entity) (loc/y entity))
    (setf (blt:color) (renderable/color entity)
          (blt:cell-char x y dx dy) (renderable/glyph entity)
          (blt:cell-char (1- x) y dx (- dy 4)) #\^
          (blt:cell-char (1+ x) y dx (- dy 4)) #\^)))


(defun blit-background-tile (x y bottom?)
  (multiple-value-bind (sx sy)
      (screen-coords x y)
    (setf
      ;; shittastic hack because BLT fucks up the spacing when
      ;; drawing an opaque background on non-1x1 fonts
      (blt:color) (blt:blue :saturation 0.8 :value 0.4)
      (blt:cell-char sx sy) #\FULL_BLOCK
      (blt:color) (blt:blue :saturation 0.7 :value 0.9)
      (blt:cell-char sx sy) (if bottom? #\M #\W))))

(defun blit-background-column (x bottom?)
  (let ((height (terrain-height x bottom?)))
    (if bottom?
      (iterate (for y :downfrom (1- *screen-height*))
               (repeat height)
               (blit-background-tile x y bottom?))
      (iterate (for y :from 0)
               (repeat height)
               (blit-background-tile x y bottom?)))))

(defun blit-background ()
  (setf (blt:layer) 0
        (blt:font) "tile"
        (blt:color) (blt:blue :saturation 0.8 :value 0.8))
  (iterate (for x :from *camera-x*)
           (repeat *screen-width*)
           (blit-background-column x t)
           (blit-background-column x nil)))


(defun blit ()
  (blt:clear)
  (setf (blt:font) "tile"
        (blt:composition) t)
  (blit-background)
  (blit-player *player*)
  (blt:refresh))


;;;; Input --------------------------------------------------------------------
(defun event ()
  (if (blt:has-input-p)
    (blt:key-case (blt:read)
      ((or (:up    :down) (:w :down)) '(:keydown :up))
      ((or (:left  :down) (:a :down)) '(:keydown :left))
      ((or (:down  :down) (:s :down)) '(:keydown :down))
      ((or (:right :down) (:d :down)) '(:keydown :right))
      ((or (:up    :up)   (:w :up))   '(:keyup :up))
      ((or (:left  :up)   (:a :up))   '(:keyup :left))
      ((or (:down  :up)   (:s :up))   '(:keyup :down))
      ((or (:right :up)   (:d :up))   '(:keyup :right))
      (:f1 '(:regen))
      (:escape '(:quit))
      (:close '(:quit)))
    :done))

(defun handle-event (event)
  (ecase (first event)
    (:quit (setf *running* nil))
    (:regen (initialize))
    (:keydown (setf (gethash (second event) *inputs*) t))
    (:keyup (setf (gethash (second event) *inputs*) nil))))

(defun handle-events ()
  (iterate
    (for event = (event))
    (until (eql event :done))
    (when event
      (handle-event event))))


;;;; Main ---------------------------------------------------------------------
(defun initialize ()
  (clrhash *inputs*)
  (clear-entities)
  (setf *running* t
        *camera-x* 0
        *camera-y* 0
        *terrain-seed* (random 500000.0)
        *player* (make-player)))

(defun run ()
  (blt:with-terminal
    (config)
    (initialize)
    (iterate
      (while *running*)
      (blit)
      (handle-events)
      (timing real-time :per-iteration-into delta-time)
      (tick (/ delta-time internal-time-units-per-second 1.0)))))


;;;; Entry --------------------------------------------------------------------
(defmacro with-open-file-dammit ((stream filespec &rest options) &body body)
  `(let ((,stream (open ,filespec ,@options)))
     (unwind-protect (progn ,@body)
       (when ,stream (close ,stream)))))


(defun main ()
  (sb-ext:disable-debugger)
  (setf *random-state* (make-random-state t))
  (with-open-file-dammit (*error-output* "/Users/sjl/src/batty/errors.log"
                                         :direction :output
                                         :if-exists :supersede)
    (with-open-file-dammit (*standard-output* "/Users/sjl/src/batty/out.log"
                                              :direction :output
                                              :if-exists :supersede)
      (run)))
  (sb-ext:exit :code 0))


;;;; Scratch ------------------------------------------------------------------
;; (setf *running* nil)
