;;; tlh-sound.el --- A very simple sound framework

;; Copyright (C) 2010 tlh <thunkout@gmail.com>

;; File:      tlh-sound.el
;; Author:    tlh <thunkout@gmail.com>
;; Created:   2010-08-13
;; Version:   1.0
;; Keywords:  sound multimedia
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;

;;; Commentary:
;;
;; A very simple sound framework.
;;

;;; Installation:
;;
;;  - put `tlh-sound.el` on your load path
;;
;;  - add these lines to your `.emacs` file:
;;
;;         (require 'tlh-sound)
;;

;;; Configuration:
;;
;;  - Set play-sound-function
;;
;;  - Set ring-bell-function
;;

;;; Code:

(eval-when-compile
  (require 'cl))

;; Customization

(defgroup tlh-sound nil
  "A very simple sound framework."
  :group 'multimedia
  :version "1.0")

(defcustom play-sound-function nil
  "A function that takes one argument, a fully-qualified
  filename, and plays the file."
  :type 'function
  :group 'tlh-sound)

(defcustom ding-sound nil
  "Audio file played by `pretty-ding'."
  :type 'file
  :group 'tlh-sound)

(defcustom quiet-functions nil
  "List of function symbols on which `pretty-ding' shouldn't
  ding."
  :type 'list
  :group 'tlh-sound)

(defcustom yell-sound nil
  "Audio file played by `yell-at-me'."
  :type 'file
  :group 'tlh-sound)

(defcustom system-volume 30
  "System audio volume [0-100]"
  :type 'integer
  :group 'tlh-sound)

(defcustom system-volume-muted nil
  "Boolean mute value."
  :type 'boolean
  :group 'tlh-sound)

(defcustom set-volume-function nil
  "A function of one argument, a number [0-100],
that sets the system volume."
  :type 'function
  :group 'tlh-sound)

(defcustom get-volume-function nil
  "A funtion of no arguments that returns the system volume as a
number [0-100]."
  :type 'function
  :group 'tlh-sound)

(defcustom volume-increment 5
  "Default increment of `system-volume' [0-100]."
  :type 'integer
  :group 'tlh-sound)

(defcustom volume-message t
  "Boolean controlling whether to display a volume message."
  :type 'boolean
  :group 'tlh-sound)

(defcustom volume-change-sound nil
  "File to play with `play-sound' on volume change."
  :type 'file
  :group 'tlh-sound)

(defcustom volume-play-sound-on-change nil
  "Boolean for whether to play a sound on volume change."
  :type 'boolean
  :group 'tlh-sound)

(defun play-sound (filename)
  (condition-case nil
      (funcall play-sound-function filename)
    (error "`play-sound-function' must be a function that takes a filename arg and plays it.")))

(defun get-volume-primitive (&optional no-update)
  "Get the actual system volume."
  (condition-case nil
      (funcall get-volume-function)
    (error "`get-volume-function' must be a function that returns the system volume as a number [0-100].")))

(defun set-volume-primitive (vol)
  "Set system volume to VOL. VOL is confined to [0-100]."
  (condition-case nil
      (funcall set-volume-function (confine-to 0 100 vol))
    (error "`set-volume-function' must be a function of one argument that sets the system volume.")))

(defun pretty-ding ()
  "Intended to be set as the value of `ring-bell-function'."
  (unless (memq this-command quiet-functions)
    (play-sound ding-sound)))

(defun yell-at-me (&optional msg)
  "A different bell function intended to be more annoying."
  (interactive)
  (play-sound yell-sound)
  (message (or msg (yow))))

(defun show-volume ()
  "Display `system-volume' as a text slider in the echo area."
  (interactive)
  (message "system:  %s Vol: %d%%" (slider system-volume) system-volume))

(defun get-volume ()
  "`get-volume-primitive' wrapper."
  (get-volume-primitive))

(defun set-volume (vol)
  "Set `system-volume' to VOL, conditionally call
`set-volume-primitive'."
  (interactive "nVolume [0-100]: ")
  (let ((vol (confine-to 0 100 vol)))
    (unless system-volume-muted
      (set-volume-primitive vol))
    (setq system-volume (get-volume))
    (when (and volume-play-sound-on-change
               volume-change-sound
               play-sound-function)
      (play-sound volume-change-sound))
    (and volume-message (show-volume))))

(defun mute-volume ()
  "Mute volume."
  (interactive)
  (setq system-volume-muted t)
  (set-volume-primitive 0)
  (and volume-message (show-volume)))

(defun unmute-volume ()
  "Unmute volume."
  (interactive)
  (setq system-volume-muted nil)
  (set-volume-primitive system-volume)
  (and volume-message (show-volume)))

(defun toggle-mute-volume ()
  "Toggle mute state."
  (interactive)
  (if system-volume-muted
      (unmute-volume)
    (mute-volume)))

(defun increase-volume (&optional inc)
  "Increase `system-volume' by INC or `volume-increment'."
  (interactive)
  (set-volume (+ system-volume
                 (or inc volume-increment))))

(defun decrease-volume (&optional dec)
  "Decrease `system-volume' by DEC or negative
`volume-increment'."
  (interactive)
  (set-volume (+ system-volume
                 (- (or dec volume-increment)))))

(provide 'tlh-sound)

;;; tlh-sound.el ends here
