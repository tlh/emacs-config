;;; osx configs

(defun osx-play-sound (filename)
  (call-process "afplay" nil 0 nil filename))

(defun osx-notify (title msg)
  (call-process "growlnotify" nil 0 nil title "-a" "Emacs" "-m" msg))

(defun osx-set-volume (vol)
  (do-applescript (format "set volume %s" (rescale vol 0 100 0 7))))

(defun osx-get-volume ()
  (do-applescript "output volume of (get volume settings)"))

(defun osx-toggle-show-hidden-files ()
  (interactive)
  (call-process "osx-toggle-show-hidden-files"))


;;; general settings

(setq mac-command-modifier         'meta
      mac-option-modifier          'hyper
      mac-function-modifier        'super
      ns-antialias-text            t
      delete-by-moving-to-trash    t
      trash-directory              (home-path ".Trash/")
      browse-url-browser-function  'browse-url-default-macosx-browser
      fortune-dir                  "/opt/local/share/games/fortune/"
      fortune-file                 "/opt/local/share/games/fortune/"
      ding-sound                   "/System/Library/Sounds/Pop.aiff"
      yell-sound                   "/System/Library/Sounds/Basso.aiff"
      volume-change-sound          "/System/Library/Sounds/Pop.aiff"
      volume-play-sound-on-change  t
      play-sound-function          'osx-play-sound
      notify-function              'osx-notify
      set-volume-function          'osx-set-volume
      get-volume-function          'osx-get-volume
      )


;;; provide

(provide 'tlh-osx)


;;; tlh-osx.el ends here
