(in-package :b)

;;;; Parameters ---------------------------------------------------------------
(defvar *running* t)

(defparameter *screen-width* 64)
(defparameter *screen-height* 48)
(defparameter *cell-size* 8)
(defparameter *assets-directory*
  (merge-pathnames #p"assets/" (deploy:data-directory)))


(defvar *player* nil)

(defvar *inputs* (make-hash-table))


(defparameter *player-velocity* 13.5) ; world cells per second
(defparameter *player-breath-time* 0.2)
(defparameter *player-breath-distance* 0.2)


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
    :renderable/glyph #\@))

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
(defun tick (delta-time)
  (tick-player *player* delta-time))


;;;; UI -----------------------------------------------------------------------
(defun screen-coord (c)
  "Translate a world coordinate into a screen coordinate.

  Returns two values:

  * The main screen coordinate
  * The pixel offset into that tile.

  "
  (multiple-value-bind (main rem)
      (truncate (* 2 c))
    (values main (truncate (* *cell-size* rem)))))


(defun blit-renderable (entity)
  (nest (multiple-value-bind (x dx) (screen-coord (loc/x entity)))
        (multiple-value-bind (y dy) (screen-coord (loc/y entity)))
        (setf (blt:color) (renderable/color entity)
              (blt:cell-char x y dx dy) (renderable/glyph entity))))

(defun blit-player (entity)
  (nest (multiple-value-bind (x dx) (screen-coord (loc/x entity)))
        (multiple-value-bind (y dy) (screen-coord (loc/y entity)))
        (setf (blt:color) (renderable/color entity)
              (blt:cell-char x y dx dy) (renderable/glyph entity)
              (blt:cell-char (1- x) y dx (- dy 4)) #\^
              (blt:cell-char (1+ x) y dx (- dy 4)) #\^)))

(defun blit ()
  (blt:clear)
  (setf (blt:color) (blt:blue) (blt:layer) 1 (blt:font) "tile")
  (dotimes (i *screen-height*)
    (blt:print 0 (* 2 i) (make-string *screen-width* :initial-element #\#)))
  (setf (blt:layer) 2)
  (blit-player *player*)
  (blt:refresh))


;;;; Input --------------------------------------------------------------------
(defun event ()
  (if (blt:has-input-p)
    (blt:key-case (blt:read)
      ((:w :down) '(:keydown :up))
      ((:a :down) '(:keydown :left))
      ((:s :down) '(:keydown :down))
      ((:d :down) '(:keydown :right))
      ((:w :up) '(:keyup :up))
      ((:a :up) '(:keyup :left))
      ((:s :up) '(:keyup :down))
      ((:d :up) '(:keyup :right))
      (:escape '(:quit))
      (:close '(:quit)))
    :done))

(defun handle-event (event)
  (ecase (first event)
    (:quit (setf *running* nil))
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
