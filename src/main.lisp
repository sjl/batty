(in-package :b)

;;;; Parameters ---------------------------------------------------------------

;;; UI
(defparameter *screen-width* 64)
(defparameter *screen-height* 48)
(defparameter *cell-size* 8)


;;; Directories
(defparameter *assets-directory*
  (merge-pathnames #p"assets/" (deploy:data-directory)))

(defparameter *music-directory*
  (merge-pathnames #p"music/" *assets-directory*))


;;; Player
(defparameter *player-velocity* 13.5) ; world cells per second


;;; Layers
(defparameter *layer-background* 0)
(defparameter *layer-bugs* 1)
(defparameter *layer-player* 2)


;;; Terrain
(defparameter *terrain-bottom-offset* 10000.0)
(defparameter *terrain-noise-scale* 0.2)
(defparameter *terrain-max-height-top* 20.0)
(defparameter *terrain-max-height-bottom* 10.0)
(defparameter *terrain-seed* 0.0)


;;;; State --------------------------------------------------------------------
(defvar *running* t)

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
  (color :initform (blt:white))
  (layer))

(define-aspect breathing
  ;; parameters
  (distance-x :initform 1.0)
  (distance-y :initform 0.0)
  (cycle-time-x :initform 0.3)
  (cycle-time-y :initform 0.3)
  ;; state
  (offset-x :initform 0)
  (offset-y :initform 0)
  (time :initform 0.0))


(defun tick-breathing (entity delta-time)
  (let ((time (+ (breathing/time entity) delta-time)))
    (setf (breathing/time entity) time

          (breathing/offset-x entity)
          (-<> time
            (sin (* <> (/ tau (breathing/cycle-time-x entity))))
            (map-range -1.0 1.0 0.0 (breathing/distance-x entity) <>)
            truncate)

          (breathing/offset-y entity)
          (-<> time
            (sin (* <> (/ tau (breathing/cycle-time-y entity))))
            (map-range -1.0 1.0 0.0 (breathing/distance-y entity) <>)
            truncate))))

(defun tick-breathing-entities (delta-time)
  (map-entities (rcurry #'tick-breathing delta-time) 'breathing))


;;;; Collision ----------------------------------------------------------------
(defun terrain-at-p (x y)
  (or (<= y (terrain-height x nil))
      (>= y (- *screen-height* 1 (terrain-height x t)))))

(defun collides-with-terrain-p (x y)
  ;; ab
  ;; 12c
  ;; 34d
  (let ((a (round (- x 0.4)))
        (b (round (+ x 0.4)))
        (c (round (- y 0.4)))
        (d (round (+ y 0.4))))
    (or (terrain-at-p a c)
        (terrain-at-p b c)
        (terrain-at-p a d)
        (terrain-at-p b d))))


;;;; Player -------------------------------------------------------------------
(define-entity player (loc renderable moveable breathing))

(defun make-player ()
  (create-entity 'player
    :renderable/glyph #\@
    :renderable/layer *layer-player*
    :breathing/cycle-time-x 1.0
    :breathing/cycle-time-y 0.3
    :breathing/distance-x 0.0
    :breathing/distance-y 3.1
    :loc/x (/ *screen-width* 2.0)
    :loc/y (/ *screen-height* 2.0)))


(defun playerp (entity)
  (eq entity *player*))


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


(defun left-of-camera-p (x)
  (< x *camera-x*))

(defun tick-player-position (player delta-time)
  (let* ((x (loc/x player))
         (y (loc/y player))
         (dx (* delta-time (moveable/vx player)))
         (dy (* delta-time (moveable/vy player)))
         (blocked-horizontally (or (collides-with-terrain-p (+ x dx) y)
                                   (left-of-camera-p (+ x dx)))))
    (unless blocked-horizontally
      (incf (loc/x player) dx)
      (incf x dx))
    (unless (collides-with-terrain-p x (+ y dy))
      (incf (loc/y player) dy))))


(defun tick-player (player delta-time)
  (tick-player-input player)
  (tick-player-position player delta-time))


;;;; Bugs ---------------------------------------------------------------------
(define-entity bug (loc renderable breathing))

(defun make-bug (x y)
  (create-entity 'bug
    :renderable/glyph #\*
    :renderable/layer *layer-bugs*
    :renderable/color (blt:hsva (random 1.0) 1.0 1.0)
    :breathing/cycle-time-x 0.2
    :breathing/cycle-time-y 0.2
    :breathing/distance-x (random 3.0)
    :breathing/distance-y (random 3.0)
    :loc/x x
    :loc/y y))


;;;; Game Logic ---------------------------------------------------------------
(defun tick-camera ()
  (when (>= (- (loc/x *player*) *camera-x*)
            (* 0.90 *screen-width*))
    (incf *camera-x* 10)))

(defun tick (delta-time)
  (tick-player *player* delta-time)
  (tick-breathing-entities delta-time)
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


(defun blit-renderable (entity)
  (setf (blt:layer) (renderable/layer entity))
  (multiple-value-bind (x y dx dy)
      (screen-coords (loc/x entity) (loc/y entity))
    (when (breathing? entity)
      (incf dx (breathing/offset-x entity))
      (incf dy (breathing/offset-y entity)))
    (setf (blt:color) (renderable/color entity)
          (blt:cell-char x y dx dy) (renderable/glyph entity))
    (when (playerp entity) ; wings
      (setf (blt:cell-char (1- x) y dx (- dy 4)) #\^
            (blt:cell-char (1+ x) y dx (- dy 4)) #\^))))

(define-system render ((entity renderable))
  (blit-renderable entity))


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
  (setf (blt:layer) *layer-background*
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
  (run-render)
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


;;;; Audio --------------------------------------------------------------------
(defvar *music* nil)

(defun random-song ()
  (random-elt (directory (make-pathname :name :wild :type "mp3"
                                        :defaults *music-directory*))))

(defmacro in-harmony (&body body)
  `(unwind-protect
     (progn
       (harmony-simple:start)
       ,@body)
     (harmony-simple:stop)))

(defmacro with-music (&body body)
  `(unwind-protect
     (progn
       (setf *music* (harmony-simple:play (random-song) :music))
       ,@body)
     (progn
       (harmony-simple:stop *music*)
       (setf *music* nil))))


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
  (in-harmony
    (blt:with-terminal
      (config)
      (initialize)
      (with-music
        (iterate
          (while *running*)
          (blit)
          (handle-events)
          (timing real-time :per-iteration-into delta-time)
          (tick (/ delta-time internal-time-units-per-second 1.0)))))))


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
;; (harmony-simple:start)
;; (harmony-simple:play (random-song-path) :music)
