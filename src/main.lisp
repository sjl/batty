(in-package :b)

;;;; Parameters ---------------------------------------------------------------
(defvar *running* t)

(defparameter *screen-width* 64)
(defparameter *screen-height* 48)
(defparameter *cell-size* 16)

(defparameter *assets-directory*
  (merge-pathnames #p"assets/" (deploy:data-directory)))


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
  (blt:set "input.filter = keyboard")
  (config-fonts))


;;;; UI -----------------------------------------------------------------------
(defun blit ()
  (blt:clear)
  (blt:print 0 0 "[font=text]Hello!")
  (blt:refresh))

;;;; Main ---------------------------------------------------------------------
(defun run ()
  (blt:with-terminal
    (setf (blt:color) (blt:white)
          *running* t)
    (config)
    (blit)
    (sleep 10)))


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

