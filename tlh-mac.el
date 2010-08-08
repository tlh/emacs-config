(set-frame-font "Menlo-12")
;; (set-frame-font "DejaVu Sans Mono-12")

(setq mac-command-key-is-meta      t
      mac-option-key-is-meta       nil
      mac-command-modifier         'meta
      mac-option-modifier          'super
      browse-url-browser-function  'browse-url-default-macosx-browser
      ding-sound                   "/System/Library/Sounds/Pop.aiff"
      yell-sound                   "/System/Library/Sounds/Basso.aiff"
      fortune-dir                  "/opt/local/share/games/fortune/"
      fortune-file                 "/opt/local/share/games/fortune/"
      trash-directory              (home-path ".Trash/")
      delete-by-moving-to-trash    t
      ns-antialias-text            t
      )

(defun play-sound (filename)
  (call-process "afplay" nil 0 nil filename))

(defun notify (title msg)
  (call-process "growlnotify" nil 0 nil title "-a" "Emacs" "-m" msg))

(provide 'tlh-mac)
