;;; osx configs

;; (add-path (site-path "osx-osascript"))
;; (require 'osx-osascript)

(defun osascript-run-str-and-return-value (str)
  (with-temp-buffer
    (apply #'call-process
           "osascript" nil (current-buffer) t (mapcar (lambda (s) (concat "-e " s))
                                                      (split-string str "\n" t)))
    (buffer-substring (point-min) (point-max))))

(defun osascript-run-str (str)
  (apply #'call-process
         "osascript" nil 0 t (mapcar (lambda (s) (concat "-e " s))
                                     (split-string str "\n" t))))

(defun osx-play-sound (filename)
  (call-process "afplay" nil 0 nil filename))

(defun osx-notify (title msg)
  (call-process "growlnotify" nil 0 nil title "-a" "Emacs" "-m" msg))

;; (defun osx-set-volume (vol)
;;   (osascript-run-str
;;    (format "set volume %s" (rescale vol 0 100 0 7))))

(defun osx-set-volume (vol)
  (let ((script (format "set volume %s" (rescale vol 0 100 0 7))))
    (call-process "osascript" nil 0 nil (concat "-e " script))))

;; (osascript-run-str "output volume of (get volume settings)")
;; (osascript-run-str "set volume 7")

(defun osx-toggle-show-hidden-files ()
  (interactive)
  (call-process "osx-toggle-show-hidden-files"))

;; font

(set-frame-font "Menlo-12")
;; (set-frame-font "DejaVu Sans Mono-12")
;; (set-frame-font "-microsoft-Comic Sans MS-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1" nil)
;; general settings

(setq mac-command-key-is-meta      t
      mac-option-key-is-meta       nil
      mac-command-modifier         'meta
      mac-option-modifier          'super
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
      )

;; provide

(provide 'tlh-osx)

;;; tlh-osx.el ends here
