(in-package :b)

;;;; Parameters ---------------------------------------------------------------
(defvar *running* t)

(defparameter *screen-width* 64)
(defparameter *screen-height* 48)
(defparameter *cell-size* 16)
(defparameter *assets-directory*
  (merge-pathnames #p"assets/" (deploy:data-directory)))

(defvar *player* nil)

(defvar *inputs* (make-hash-table))


;;;; Config -------------------------------------------------------------------
(defun asset-path (filename)
  (-<> filename
    (merge-pathnames (pathname <>) *assets-directory*)
    (namestring <>)))

(defun config-fonts ()
  (blt:set "font: ~A, size=~Dx~:*~D, spacing=2x2;"
           (asset-path "ProggySquare/ProggySquare.ttf")
           *cell-size*)
  (blt:set "tile font: ~A, size=~Dx~:*~D, spacing=1x1;"
           (asset-path "ProggySquare/ProggySquare.ttf")
           (floor *cell-size* 2))
  (blt:set "text font: ~A, size=~Dx~:*~D, spacing=1x2;"
           (asset-path "UbuntuMono/UbuntuMono-R.ttf")
           *cell-size*))

(defun config ()
  (assert (evenp *cell-size*))
  (blt:set (format nil "window.size = ~Dx~D"
                   (* 2 *screen-width*)
                   (* 2 *screen-height*)))
  (blt:set "window.title = B@TTY")
  (blt:set "window.cellsize = ~Dx~:*~D" (floor *cell-size* 2))
  (blt:set "window.resizeable = false")
  (blt:set "output.vsync = true")
  (blt:set "input.filter = keyboard+")
  (config-fonts))


;;;; Aspects ------------------------------------------------------------------
(define-aspect loc
  (x :initform 0.0 :type single-float)
  (y :initform 0.0 :type single-float))

(define-aspect renderable
  (glyph :initform #\? :type character)
  (color :initform (blt:white)))


;;;; Player -------------------------------------------------------------------
(define-entity player (loc renderable))

(defun make-player ()
  (create-entity 'player
    :renderable/glyph #\@))

(defun tick-player (player)
  (when (gethash :up *inputs*) (decf (loc/y player)))
  (when (gethash :down *inputs*) (incf (loc/y player)))
  (when (gethash :left *inputs*) (decf (loc/x player)))
  (when (gethash :right *inputs*) (incf (loc/x player))))


;;;; Game Logic ---------------------------------------------------------------
(defun tick ()
  (tick-player *player*))


;;;; UI -----------------------------------------------------------------------
(defun blit-player (player)
  (setf
    (blt:color) (renderable/color player)
    (blt:cell-char (* 2 (truncate (loc/x player)))
                   (* 2 (truncate (loc/y player))))
    (renderable/glyph player)))

(defun blit ()
  (blt:clear)
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
      (tick))))


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
