;;; emms config

(add-path (site-path "emms/lisp"))
(require 'emms-setup)
(require 'emms-browser)
(emms-standard)

(defalias 'current-track 'emms-playlist-current-selected-track)

;; utility functions

(defun tlh-emms-track-description (track)
  (apply 'format "%s - [%s] %s - %s"
         (mapcar (lambda (p) (emms-track-get track p))
                 '(info-artist info-year info-album info-title))))

;; seek slider

(defvar emms-player-output-buffer nil
  "Output buffer for the emms player process")

(defvar emms-seek-slider-length 100
  "Length of the seek slider")

;; Redefine `emms-player-simple-start' to get a process buffer
(defun emms-player-simple-start (filename player cmdname params)
  (let ((process (apply 'start-process
                        emms-player-simple-process-name
                        emms-player-output-buffer
                        cmdname
                        (append params (list filename)))))
    (set-process-sentinel process 'emms-player-simple-sentinel))
  (emms-player-started player))

(defun emms-display-track-position-slider (&optional len)
  (let* ((percent (emms-track-position))
         (len     (or len emms-seek-slider-length))
         (pos     (truncate (rescale percent 0 100 0 len)))
         (bar     (make-string (max 0 (1- pos)) ?=))
         (bkgd    (make-string (- len pos) ?-))
         (descrip (emms-track-description (current-track))))
    (message "     %s\n[%s%s%s] %s%%" descrip bar "O" bkgd percent)))

(defun emms-slider-seek (secs)
  (interactive (list (or current-prefix-arg
                         (read-from-minibuffer "Seconds: " nil nil t))))
  (if emms-player-playing-p
      (progn
        (emms-player-seek secs)
        (emms-display-track-position-slider))
    (message "Nothing is currently playing.")))

(defun emms-slider-seek-backward ()
  (interactive)
  (emms-slider-seek (- emms-seek-seconds)))

(defun emms-slider-seek-forward ()
  (interactive)
  (emms-slider-seek emms-seek-seconds))

;; mplayer stuff

(setq emms-player-mplayer-parameters '("-slave" "-quiet")
      emms-player-output-buffer       "*mplayer*")

(defun emms-mplayer-get-pos ()
  (save-current-buffer
    (set-buffer emms-player-output-buffer)
    (erase-buffer)
    (process-send-string emms-player-simple-process-name "get_percent_pos\n")
    (while (= (point-min) (point-max)) (sleep-for .01))
    (goto-char (point-min))
    (ignore-errors
      (re-search-forward "=\\(.+\\)\n")
      (read (match-string 1)))))

(defalias 'emms-track-position 'emms-mplayer-get-pos)

;; emms-info-id3v2

(add-path (elisp-path "emms-info-id3v2"))
(require 'emms-info-id3v2)

;; general settings

(setq emms-player-list                     '(emms-player-mplayer)
      emms-info-functions                  '(emms-info-id3v2)
      emms-track-description-function      'tlh-emms-track-description
      emms-source-file-default-directory    (home-path "Music/iTunes/iTunes Media/Music/")
      emms-cache-file                       (etc-path "emms/cache")
      emms-repeat-playlist                  t
      )

(provide 'tlh-emms)

;;; tlh-emms.el ends here
