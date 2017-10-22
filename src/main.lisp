(in-package :b)

;;;; Parameters ---------------------------------------------------------------

;;; UI
(defparameter *screen-width* 64)
(defparameter *screen-height* 40)
(defparameter *cell-size* 10)
(defparameter *offscreen-buffer* 10)

(defparameter *camera-scroll-rate* 40)
(defparameter *camera-scroll-boundary* 0.9)


;;; Directories
(defparameter *assets-directory*
  (merge-pathnames #p"assets/" (deploy:data-directory)))

(defparameter *music-directory*
  (merge-pathnames #p"music/" *assets-directory*))

(defparameter *sounds-directory*
  (merge-pathnames #p"sounds/" *assets-directory*))


;;; SFX
(defparameter *sound-chomp*
  (merge-pathnames #p"chomp.mp3" *sounds-directory*))


;;; Player
(defparameter *player-max-horizontal-velocity* 13.5)
(defparameter *player-horizontal-acceleration* 50.0)
(defparameter *player-horizontal-friction* 0.95)

(defparameter *player-max-vertical-velocity* 15.0)
(defparameter *player-flap-velocity* -15.0)
(defparameter *player-gravity* 30.0)

(defparameter *player-flap-cooldown* 0.2) ; seconds

(defparameter *score* 0)


;;; Layers
(defparameter *layer-background* 0)
(defparameter *layer-bugs* 1)
(defparameter *layer-player* 2)
(defparameter *layer-squeak* 3)
(defparameter *layer-hud* 4)


;;; Terrain
(defparameter *terrain-bottom-offset* 10000.0)
(defparameter *terrain-noise-scale* 0.2)
(defparameter *terrain-max-height-top* 20.0)
(defparameter *terrain-max-height-bottom* 10.0)
(defparameter *terrain-seed* 0.0)


;;; Squeak
(defparameter *squeak-cooldown* 0.3) ; seconds
(defparameter *squeak-velocity* 40.0) ; world cells per second
(defparameter *squeak-radar-scale* 7.0) ; width of the radar circle thing
(defparameter *squeak-mobs-scale* 30.0)
(defparameter *squeak-bg-scale* 30.0)
(defparameter *squeak-radar-opacity* 0.7)


;;; Jankass Hyperlinks
(defparameter *music-link* (blt:rgba #x88 #x88 #xFF))
(defparameter *sound-link* (blt:rgba #x88 #x88 #xFE))


;;;; State --------------------------------------------------------------------
(defvar *running* t)

(defvar *player* nil)
(defvar *inputs* (make-hash-table))
(defvar *camera-x* 0)
(defvar *camera-y* 0)
(defvar *seconds-since-squeak* 0.0)
(defvar *squeak-radius* 0.0)
(defvar *squeak-origin-x* 0)
(defvar *squeak-origin-y* 0)


;;;; Config -------------------------------------------------------------------
(defun asset-path (filename)
  (-<> filename
    (merge-pathnames (pathname <>) *assets-directory*)
    (namestring <>)))

(defun config-fonts ()
  (blt:set "font: ~A, size=~Dx~:*~D, spacing=2x2;"
           (asset-path "ProggySquare/ProggySquare.ttf")
           (* 2 *cell-size*))
  (blt:set "tiny font: ~A, size=~Dx~:*~D, spacing=1x1, mode=monochrome;"
           (asset-path "ProggySquare/ProggySquare.ttf")
           *cell-size*)
  (blt:set "tile font: ~A, size=~Dx~:*~D, spacing=2x2, mode=monochrome;"
           (asset-path "ProggySquare/ProggySquare.ttf")
           (* 2 *cell-size*))
  (blt:set "text font: ~A, size=~Dx~D, spacing=1x2;"
           (asset-path "UbuntuMono/UbuntuMono-R.ttf")
           *cell-size*
           (* 2 *cell-size*)))

(defun config ()
  (assert (evenp *cell-size*))
  (blt:set (format nil "window.size = ~Dx~D"
                   (* 2 *screen-width*)
                   (* 2 *screen-height*)))
  (blt:set "window.title = B@TTY")
  (blt:set "window.cellsize = ~Dx~:*~D" *cell-size*)
  (blt:set "window.resizeable = false")
  (blt:set "output.vsync = true")
  (blt:set "input.filter = keyboard+,mouse")
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
  (ax :initform 0.0 :type single-float)
  (ay :initform 0.0 :type single-float)
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


(define-system clear-offscreen ((entity loc))
  (when (< (loc/x entity)
           (- *camera-x* *offscreen-buffer*))
    (destroy-entity entity)))


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

(define-entity player (loc renderable moveable breathing)
  (squeak-cooldown :accessor player/squeak-cooldown :initform 0.0)
  (flap-cooldown :accessor player/flap-cooldown :initform 0.0))

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


(defun squeakablep (player)
  (zerop (player/squeak-cooldown player)))

(defun squeak (player)
  (setf *seconds-since-squeak* 0.0
        *squeak-radius* 0.0
        *squeak-origin-x* (loc/x player)
        *squeak-origin-y* (loc/y player)
        (player/squeak-cooldown player) *squeak-cooldown*))


(defun flappablep (player)
  (zerop (player/flap-cooldown player)))

(defun flap (player)
  (setf (player/flap-cooldown player) *player-flap-cooldown*
        (moveable/vy player) *player-flap-velocity*))


(defun tick-player-squeak (player delta-time)
  (zapf (player/squeak-cooldown player)
        (max 0.0 (- % delta-time))))

(defun tick-player-flap (player delta-time)
  (zapf (player/flap-cooldown player)
        (max 0.0 (- % delta-time))))


(defun tick-player-input (player)
  (let ((l? (gethash :left *inputs*))
        (r? (gethash :right *inputs*))
        (flap? (gethash :flap *inputs*))
        (squeak? (gethash :squeak *inputs*)))
    ;; squeaking
    (when (and squeak? (squeakablep player))
      (squeak player))
    (when (and flap? (flappablep player))
      (flap player))
    (setf
      ;; vertical movement (flaps)
      (moveable/ay player) *player-gravity*
      ;; horizontal movement
      (moveable/ax player) (cond ((and l? r?) 0.0)
                                 (l? (- *player-horizontal-acceleration*))
                                 (r? *player-horizontal-acceleration*)
                                 (t 0.0)))))

(defun left-of-camera-p (x)
  (< x *camera-x*))


(defun clamp-player-velocity-horizontal (v)
  (clamp (- *player-max-horizontal-velocity*)
         v
         *player-max-horizontal-velocity*))

(defun clamp-player-velocity-vertical (v)
  (clamp (- *player-max-vertical-velocity*)
         v
         *player-max-vertical-velocity*))

(defun tick-player-position (player delta-time)
  (let* ((x (loc/x player))
         (y (loc/y player))
         (ax (moveable/ax player))
         (ay (moveable/ay player))
         (vx (zapf (moveable/vx player)
                   (-<> (+ % (* delta-time ax))
                     clamp-player-velocity-horizontal
                     (* *player-horizontal-friction* <>))))
         (vy (zapf (moveable/vy player)
                   (+ % (* delta-time ay))))
         (dx (* delta-time vx))
         (dy (* delta-time vy))
         (blocked-horizontally? (or (collides-with-terrain-p (+ x dx) y)
                                    (left-of-camera-p (+ x dx)))))
    ;; (pr x y)
    ;; (pr ax ay (* delta-time ay))
    ;; (pr vx vy)
    ;; (pr dx dy)
    ;; (pr '---------------)
    (unless blocked-horizontally?
      (incf (loc/x player) dx)
      (incf x dx))
    (unless (collides-with-terrain-p x (+ y dy))
      (incf (loc/y player) dy))))

(defun tick-player (player delta-time)
  (tick-player-squeak player delta-time)
  (tick-player-flap player delta-time)
  (tick-player-input player)
  (tick-player-position player delta-time))


;;;; Bugs ---------------------------------------------------------------------
(define-entity bug (loc renderable breathing))

(defun make-bug (x y)
  (create-entity 'bug
    :renderable/glyph #\*
    :renderable/layer *layer-bugs*
    :renderable/color (blt:hsva (random 1.0) 1.0 1.0)
    :breathing/cycle-time-x (random-range 0.2 1.0)
    :breathing/cycle-time-y (random-range 0.2 1.0)
    :breathing/time (random 10.0)
    :breathing/distance-x (random-range 1.0 4.0)
    :breathing/distance-y (random-range 1.0 4.0)
    :loc/x x
    :loc/y y))


;;;; Chunk Generation ---------------------------------------------------------
(defparameter *chunk-size* 100)
(defparameter *bugs-per-chunk* 10)
(defvar *current-chunk* -1)


(defun chunk (x)
  (truncate x *chunk-size*))

(defun chunk-needed (x)
  "Return the rightmost chunk that needs to be generated give a player at `x`."
  (1+ (chunk x)))

(defun chunk-x (chunk x)
  (+ (* chunk *chunk-size*) x))

(defun chunk-random-x (chunk)
  (chunk-x chunk (random *chunk-size*)))

(defun chunk-random-y (x)
  (random-range (+ (float (terrain-height x nil)) 1)
                (- *screen-height* (terrain-height x t) 1)))

(defun chunk-random-coords (chunk)
  (let* ((x (chunk-random-x chunk))
         (y (chunk-random-y x)))
    (values x y)))


(defun generate-chunk-bugs (chunk)
  (iterate (repeat *bugs-per-chunk*)
           (for (values x y) = (chunk-random-coords chunk))
           (make-bug x y)))

(defun generate-chunk (chunk)
  (generate-chunk-bugs chunk))


(defun ensure-chunk (x)
  (iterate (for chunk :from *current-chunk* :to (chunk-needed x))
           (generate-chunk chunk)
           (finally (maxf *current-chunk* chunk))))


;;;; Game Logic ---------------------------------------------------------------
(defparameter *bug-eating-range* 1.0)


(defun distance (x1 y1 x2 y2)
  (sqrt (+ (square (- x1 x2))
           (square (- y1 y2)))))

(defun bug-within-range-p (player bug)
  (<= (distance (loc/x player) (loc/y player)
                (loc/x bug) (loc/y bug))
      *bug-eating-range*))

(defun eat-bugs ()
  (let ((bugs (remove-if-not (curry #'bug-within-range-p *player*)
                             (map-entities #'identity 'bug))))
    (when bugs
      (play-chomp)
      (incf *score* (length bugs))
      (map nil #'destroy-entity bugs))))


(defun tick-squeak (delta-time)
  (incf *seconds-since-squeak* delta-time)
  (setf *squeak-radius* (* *squeak-velocity* *seconds-since-squeak*)))

(defun tick-camera ()
  (when (>= (- (loc/x *player*) *camera-x*)
            (* *camera-scroll-boundary* *screen-width*))
    (incf *camera-x* *camera-scroll-rate*)))

(defun tick (delta-time)
  (run-clear-offscreen)
  (eat-bugs)
  (tick-player *player* delta-time)
  (tick-squeak delta-time)
  (tick-breathing-entities delta-time)
  (tick-camera)
  (ensure-chunk (loc/x *player*)))


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

(defun world-coords (sx sy)
  "Translate screen coordinates into world coordinates."
  (values (+ *camera-x* sx)
          (+ *camera-y* sy)))


(defun-inline alpha-patch (color alpha)
  ;; ugly hack taking advantage of the internal fact that blt stores the alpha
  ;; in the high-order byte of the color
  (dpb (truncate (* 255 alpha)) (byte 8 24) color))

(defun blit-renderable (entity)
  (let* ((x (loc/x entity))
         (y (loc/y entity))
         (alpha (squeak-alpha x y *squeak-mobs-scale*))
         (player? (playerp entity)))
    (when (or player? (<= 0.0 alpha 1.0))
      (setf (blt:layer) (renderable/layer entity))
      (multiple-value-bind (x y dx dy) (screen-coords x y)
        (when (breathing? entity)
          (incf dx (breathing/offset-x entity))
          (incf dy (breathing/offset-y entity)))
        (setf (blt:color) (alpha-patch (renderable/color entity)
                                       (if player? 1.0 alpha))
              (blt:cell-char x y dx dy) (renderable/glyph entity))
        (when (playerp entity) ; wings
          (let ((f (if (flappablep entity) 0 6)))
            (setf (blt:cell-char (1- x) y dx (+ dy f -4)) #\^
                  (blt:cell-char (1+ x) y dx (+ dy f -4)) #\^)))))))

(define-system render ((entity renderable))
  (blit-renderable entity))


(defun blit-background-tile (x y bottom?)
  (let ((alpha (squeak-alpha x y *squeak-bg-scale*)))
    (when (<= 0.0 alpha 1.0)
      (multiple-value-bind (sx sy)
          (screen-coords x y)
        (setf
          ;; shittastic hack because BLT fucks up the spacing when
          ;; drawing an opaque background on non-1x1 fonts
          (blt:color) (blt:blue :saturation 0.8 :value 0.4 :alpha alpha)
          (blt:cell-char sx sy) #\FULL_BLOCK
          (blt:color) (blt:blue :saturation 0.7 :value 0.9 :alpha alpha)
          (blt:cell-char sx sy) (if bottom? #\M #\W))))))

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


(defun print-with-background (x y fg bg string)
  ;; lame, blt
  (setf (blt:color) bg)
  (blt:print x y (make-string (length string) :initial-element #\full_block))
  (setf (blt:color) fg)
  (blt:print x y string))

(defun blit-hud ()
  (setf (blt:layer) *layer-hud*
        (blt:font) "text")
  (print-with-background 0 0 (blt:white) (blt:black)
                         (format nil "BUGS EATEN: ~D" *score*)))


(defun distance-to-squeak-origin (x y)
  (distance x y *squeak-origin-x* *squeak-origin-y*))

(defun distance-to-squeak (x y)
  (- *squeak-radius* (distance-to-squeak-origin x y)))

(defun squeak-alpha (x y scale)
  (map-range 0.0 scale 1.0 0.0 (distance-to-squeak x y)))


(defun blit-squeak ()
  (setf (blt:font) "tile")
  (iterate
    (for-nested ((sx :from 0 :below *screen-width*)
                 (sy :from 0 :below *screen-height*)))
    (for (values x y) = (world-coords sx sy))
    (for alpha = (squeak-alpha x y *squeak-radar-scale*))
    (when (<= 0.0 alpha 1.0)
      (setf (blt:color) (blt:white :alpha (* *squeak-radar-opacity* (square alpha)))
            (blt:cell-char (* 2 sx) (* 2 sy)) #\full_block))))

(defun blit ()
  (blt:clear)
  (setf (blt:font) "tile"
        (blt:composition) t)
  (blit-background)
  (run-render)
  (blit-squeak)
  (blit-hud)
  (blt:refresh))


(defun blit-file-to-screen (file)
  (blt:clear)
  (setf (blt:font) "text")
  (blt:print 0 0 (read-file-into-string (asset-path file))
             :width (* 2 *screen-width*)
             :height (* 2 *screen-height*)
             :halign :center
             :valign :center)
  (blt:refresh))


;;;; Input --------------------------------------------------------------------
(defun event ()
  (if (blt:has-input-p)
    (blt:key-case (blt:read)
      ((:m :down) '(:keydown :squeak))
      ((:m :up)   '(:keyup   :squeak))
      ((:n :down) '(:keydown :flap))
      ((:n :up)   '(:keyup   :flap))
      ((:z :down) '(:keydown :left))
      ((:z :up)   '(:keyup   :left))
      ((:x :down) '(:keydown :right))
      ((:x :up)   '(:keyup   :right))
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


;;;; Harmony ------------------------------------------------------------------
(defclass sine-source (harmony:source)
  ((pos :initform 0.0 :accessor pos)
   (hz :initform 440.0 :accessor hz)))

(defmethod initialize-instance :after ((source sine-source) &key)
  (setf (harmony:decoder source) 'decode))

(defmethod harmony:initialize-channel ((source sine-source))
  (cl-mixed:make-channel nil 4096 :float 1 :alternating 44100))

(defmethod seek-to-sample ((source sine-source) position)
  (setf (pos source) position))

(defun time-per-sample (sample-rate hz)
  (* (coerce tau 'single-float) hz (/ sample-rate)))

(defun decode (samples source)
  (let ((buffer (cl-mixed:data (cl-mixed:channel source)))
        (time-per-sample (time-per-sample (cl-mixed:samplerate source)
                                          (hz source))))
    (iterate
      (with start-time = (pos source))
      (for time :from start-time :by time-per-sample)
      (for i :from 0 :below samples)
      (for s = (sin time))
      (setf (cffi:mem-aref buffer :float i) s)
      (finally (setf (pos source)
                     (mod time (coerce tau 'single-float)))))))

;; (harmony-simple:start)
;; (harmony-simple:play "assets/sounds/chomp.mp3" :sfx)
;; (harmony-simple:stop)

;; (defparameter *sine*
;;   (make-instance 'sine-source
;;                  :server harmony-simple:*server*
;;                  :paused nil
;;                  :loop nil
;;                  :location nil
;;                  :velocity nil))

;; (setf (harmony:volume *sine*) 0.2)

;; (defparameter *mix* (harmony:segment :sfx harmony-simple:*server*))

;; (harmony:add *sine* *mix*)
;; (harmony:withdraw *sine* *mix*)


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


(defun play-chomp ()
  (harmony-simple:play *sound-chomp* :sfx))


;;;; Splash -------------------------------------------------------------------
(defun open-link (link)
  ;; something shits the bed if you try to do this on the game loop thread
  ;; computers are garbage
  (bt:make-thread (lambda ()
                    (uiop:run-program (list "/usr/bin/open" link)
                                      :ignore-error-status t))))

(defun link-clicked ()
  ;; don't judge me
  (let ((color (blt:cell-color (blt:mouse-x) (blt:mouse-y))))
    (cond
      ((= color *music-link*)
       "http://freemusicarchive.org/music/Rolemusic/The_Black_Dot/")
      ((= color *sound-link*)
       "https://freesound.org/people/timgormly/packs/10094/")
      (t nil))))

(defun controls-screen ()
  (blit-file-to-screen "controls.txt")
  (iterate
    (if (blt:has-input-p)
      (blt:key-case (blt:read)
        ((or :escape :close) (return-from controls-screen))
        ((or :m :space) (return)))
      (blt:sleep 1/60))
    (blt:refresh))
  (game-loop))

(defun splash-screen ()
  (iterate
    (blit-file-to-screen "splash.txt")
    (if (blt:has-input-p)
      (blt:key-case (blt:read)
        ((or :q :escape :close) (return-from splash-screen))
        ((or :p :space) (controls-screen))
        (:mouse-left (when-let ((link (link-clicked)))
                       (open-link link))))
      (blt:sleep 1/60))))


;;;; Main ---------------------------------------------------------------------
(defun initialize ()
  (clrhash *inputs*)
  (clear-entities)
  (setf *running* t
        *camera-x* 0
        *camera-y* 0
        *current-chunk* -1
        *player* (make-player)
        *squeak-origin-x* (loc/x *player*)
        *squeak-origin-y* (loc/y *player*)
        *seconds-since-squeak* 0.0
        *score* 0
        *terrain-seed* (random 500000.0)))

(defun game-loop ()
  (initialize)
  (iterate
    (while *running*)
    (blit)
    (handle-events)
    (timing real-time :per-iteration-into delta-time)
    (tick (/ delta-time internal-time-units-per-second 1.0))))

(defun run ()
  (in-harmony
    (blt:with-terminal
      (config)
      (with-music
        (splash-screen)))))


;;;; Entry --------------------------------------------------------------------
(defmacro with-open-file-dammit ((stream filespec &rest options) &body body)
  `(let ((,stream (open ,filespec ,@options)))
     (unwind-protect (progn ,@body)
       (when ,stream (close ,stream)))))


(defun main ()
  (sb-ext:disable-debugger)
  (setf *random-state* (make-random-state t))
  (with-open-file-dammit (*error-output* (asset-path "errors.log")
                                         :direction :output
                                         :if-exists :supersede)
    (with-open-file-dammit (*standard-output* (asset-path "output.log")
                                              :direction :output
                                              :if-exists :supersede)
      (pr 'started)
      (run)))
  (sb-ext:exit :code 0))


;;;; Scratch ------------------------------------------------------------------
