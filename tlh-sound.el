;;; sound framework

;; play sound

(defvar play-sound-function nil
  "A function that takes one argument, a fully-qualified
  filename, and plays the file.")

(defun play-sound (filename)
  (funcall play-sound-function filename))

;; ding

(defvar ding-sound nil
  "Audio file played by `pretty-ding'.")

(defvar quiet-functions nil
  "List of functions on which `pretty-ding' shouldn't ding.")

(defvar yell-sound nil
  "Audio file played by `yell-at-me'.")

(defun pretty-ding ()
  (unless (memq this-command quiet-functions)
    (play-sound ding-sound)))

(defun yell-at-me (&optional msg)
  (interactive)
  (play-sound yell-sound)
  (message (or msg (yow))))

;; system volume

(defvar system-volume 30 "System audio volume [0-100]")

(defvar system-volume-muted nil "Boolean mute value.")

(defvar set-volume-function nil
  "A function of one argument, a number [0-100],
and sets the system volume accordingly.")

(defvar volume-increment 5
  "Default increment of `system-volume' [0-100].")

(defvar volume-message t
  "Boolean controlling whether to display a volume message.")

(defvar volume-change-sound nil
  "File to play with `play-sound' on volume change.")

(defvar volume-play-sound-on-change nil
  "Boolean for whether to play a sound on volume change.")

(defun show-volume ()
  (interactive)
  (let* ((pos    (truncate system-volume))
         (slider (make-string (1- pos) ?=))
         (bkgd   (make-string (- 100 pos) ?-))
         (knob   (if system-volume-muted "M" "O")))
    (message "[%s%s%s] Vol: %s"
             slider
             knob
             bkgd
             system-volume)))

(defun set-volume-primitive (vol &optional no-update)
  (let ((vol (confine-to 0 100 vol)))
    (condition-case nil
        (funcall set-volume-function vol)
      ((wrong-number-of-arguments invalid-function void-function)
       (error "`set-volume-function' must be a function of one argument that sets the system volume.")))
    (unless no-update (setq system-volume vol))))

(defun set-volume (vol)
  (interactive "nVolume [0-100]: ")
  (let ((vol (confine-to 0 100 vol)))
    (unless system-volume-muted
      (set-volume-primitive vol))
    (setq system-volume vol)
    (when (and volume-play-sound-on-change
               volume-change-sound
               play-sound-function)
      (play-sound volume-change-sound))
    (and volume-message (show-volume))))

(defun mute-volume ()
  (interactive)
  (setq system-volume-muted t)
  (set-volume-primitive 0 t)
  (and volume-message (show-volume)))

(defun unmute-volume ()
  (interactive)
  (setq system-volume-muted nil)
  (set-volume-primitive system-volume t)
  (and volume-message (show-volume)))

(defun toggle-mute-volume ()
  (interactive)
  (if system-volume-muted
      (unmute-volume)
    (mute-volume)))

(defun increase-volume (&optional inc)
  (interactive)
  (set-volume (+ system-volume
                 (or inc volume-increment))))

(defun decrease-volume (&optional dec)
  (interactive)
  (set-volume (+ system-volume
                 (- (or dec volume-increment)))))

(provide 'tlh-sound)

;;; tlh-sound.el ends here
