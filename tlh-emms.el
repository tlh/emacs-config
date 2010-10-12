;;;
;;;   emms config
;;;


(add-paths (site-path  "emms/lisp")
           (elisp-path "emms-info-id3v2"))


;;; requires

(require 'emms-source-file)
(require 'emms-source-playlist)
(require 'emms-player-simple)
(require 'emms-player-mplayer)
(require 'emms-playlist-mode)
(require 'emms-info-mp3info)
(require 'emms-info-ogginfo)
(require 'emms-info-id3v2)
(require 'emms-mode-line)
(require 'emms-mode-line-icon)
(require 'emms-cache)
(require 'emms-browser)


;;; utility functions

(defalias 'current-track 'emms-playlist-current-selected-track)

(defun tlh-emms-track-description (&optional track)
  (let ((track (or track (current-track))))
    (format "%s - [%s] %s - %s"
            (emms-track-get track 'info-artist)
            (emms-track-get track 'info-year)
            (emms-track-get track 'info-album)
            (emms-track-get track 'info-title))))


;;; emms process buffer

(defvar emms-player-output-buffer nil
  "Output buffer for the emms player process")

;;; Redefine `emms-player-simple-start' to get a process buffer
(defun emms-player-simple-start (filename player cmdname params)
  (let ((process (apply 'start-process
                        emms-player-simple-process-name
                        emms-player-output-buffer
                        cmdname
                        (append params (list filename)))))
    (set-process-sentinel process 'emms-player-simple-sentinel))
  (emms-player-started player))


;;; seek slider

(defvar emms-time-length-function nil
  "Funtion that returns the current track time length in seconds.")

(defun emms-time-length ()
  (funcall emms-time-length-function))

(defvar emms-time-position-function nil
  "Funtion that returns the current track position in seconds.")

(defun emms-time-position ()
  (funcall emms-time-position-function))

(defvar emms-percentage-position-function nil
  "Funtion that returns the current track position as a percentage.")

(defun emms-percentage-position ()
  (funcall emms-percentage-position-function))

(defun emms-display-track-position-slider ()
  (interactive)
  (let ((perc (emms-percentage-position))
        (secs (emms-time-position))
        (len  (emms-time-length)))
    (message "     %s\n%s %s%%  %s/%s seconds"
             (tlh-emms-track-description)
             (slider perc (- (frame-width) 40))
             perc secs len)))

(defun emms-slider-seek (secs)
  (interactive
   (list (or current-prefix-arg
             (read-from-minibuffer "Seconds: " nil nil t))))
  (if (not emms-player-playing-p)
      (message "Nothing is currently playing.")
    (emms-player-seek secs)
    (emms-display-track-position-slider)))

(defun emms-slider-seek-backward (&optional secs)
  (interactive)
  (emms-slider-seek (- (or secs emms-seek-seconds))))

(defun emms-slider-seek-forward (&optional secs)
  (interactive)
  (emms-slider-seek (or secs emms-seek-seconds)))


;;; mplayer

(defvar emms-mplayer-volume-increment 2
  "Default mplayer volume increment.")

(defun emms-mplayer-cmd (cmd &optional respond read)
  (aif (get-process emms-player-simple-process-name)
       (save-current-buffer
         (set-buffer emms-player-output-buffer)
         (erase-buffer)
         (process-send-string emms-player-simple-process-name cmd)
         (if (not respond)
             t
           (when (accept-process-output it 1)
             (goto-char (point-min))
             (ignore-errors
               (re-search-forward "=\\(.+\\)\n")
               (let ((val (match-string 1)))
                 (if read (read val) val))))))
       (message "Nothing is currently playing.")
       nil))

(defun emms-mplayer-set-volume (vol)
  (emms-mplayer-cmd
   (format "volume %s 1\n" (confine-to 0 100 vol))))

(defun emms-mplayer-step-volume (step)
  (emms-mplayer-cmd
   (format "step_property volume %s\n" step)))

(defun emms-mplayer-time-length ()
  (emms-mplayer-cmd "get_time_length\n" t t))

(defun emms-mplayer-time-position ()
  (emms-mplayer-cmd "get_time_pos\n" t t))

(defun emms-mplayer-percentage-position ()
  (emms-mplayer-cmd "get_percent_pos\n" t t))

(defun emms-mplayer-volume ()
  (emms-mplayer-cmd "get_property volume\n" t t))

(defmacro def-emms-mplayer-cmd (name cmd)
  `(defun ,(symcat 'emms-mplayer- name) ()
     (emms-mplayer-cmd ,cmd t)))

(def-emms-mplayer-cmd album   "get_meta_album\n")
(def-emms-mplayer-cmd artist  "get_meta_artist\n")
(def-emms-mplayer-cmd comment "get_meta_comment\n")
(def-emms-mplayer-cmd genre   "get_meta_genre\n")
(def-emms-mplayer-cmd title   "get_meta_title\n")
(def-emms-mplayer-cmd track   "get_meta_track\n")
(def-emms-mplayer-cmd year    "get_meta_year\n")

(defun emms-mplayer-offset-volume (offset)
  (when (emms-mplayer-step-volume offset)
    (let ((vol (emms-mplayer-volume)))
      (message "mplayer: %s Vol: %d%%"
               (slider vol (- (frame-width) 30))
               vol))))

(defun emms-mplayer-decrease-volume (&optional dec)
  (interactive)
  (emms-mplayer-offset-volume
   (- (or dec emms-mplayer-volume-increment))))

(defun emms-mplayer-increase-volume (&optional inc)
  (interactive)
  (emms-mplayer-offset-volume
   (or inc emms-mplayer-volume-increment)))


;;; force track recaching

(defun emms-cache-delete-dir (dir)
  (interactive
   (list
    (read-directory-name
     "DDirectory: " emms-source-file-default-directory)))
  (dolist (path (directory-files dir t))
    (emms-cache-del path)))

(defun emms-playlist-delete-file-from-cache ()
  (interactive)
  (let* ((track (emms-playlist-track-at (point)))
         (path  (emms-track-get track 'name)))
    (unless track (error "No track at point."))
    (emms-cache-del path)
    (next-line)
    ;; FIXME: playlist should update to show "NIL" track info
    ;; (emms-playlist-mode-kill-track)
    ;; (emms-playlist-track-updated track)
    (message "Deleted %S from cache." path)))

(fill-keymap emms-playlist-mode-map
             "M-k"   'emms-playlist-delete-file-from-cache
             "C-M-k" 'emms-cache-delete-dir)


;;; settings

(defpathfn emms-path (etc-path "emms/"))

(setq emms-player-list                         '(emms-player-mplayer)
      emms-playlist-default-major-mode         'emms-playlist-mode
      emms-track-initialize-functions          '(emms-info-initialize-track)
      emms-info-functions                      '(emms-info-id3v2 emms-info-ogginfo)
      emms-track-description-function          'tlh-emms-track-description
      emms-source-file-default-directory        (home-path "Music/iTunes/iTunes Media/Music/")
      emms-directory                            (emms-path)
      emms-cache-file                           (emms-path "cache")
      emms-repeat-playlist                      t
      emms-time-length-function                'emms-mplayer-time-length
      emms-time-position-function              'emms-mplayer-time-position
      emms-percentage-position-function        'emms-mplayer-percentage-position
      emms-player-mplayer-parameters           '("-slave" "-quiet")
      emms-player-output-buffer                 "*mplayer*"
      )

(emms-cache 1)


;;; provide

(provide 'tlh-emms)


;;; tlh-emms.el ends here
