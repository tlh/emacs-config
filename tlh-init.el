;;; non-mode-specific configs

;; server

(require 'server)
(or (server-running-p) (server-start))

;; coding system

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system       'utf-8)

;; variable configs

(setq inhibit-splash-screen                     t
      inhibit-startup-message                   t
      inhibit-startup-echo-area-message         user-login-name
      user-full-name                            "tlh"
      user-mail-address                         "thunkout@gmail.com"
      custom-file                               (elisp-path "tlh-custom.el")
      auto-save-list-file-prefix                (etc-path "auto-save-list/.saves-")
      backup-directory-alist                   `(("." . ,(home-path "tcvol1/emacs-misc/backups/")))
      yow-file                                  (etc-path "yow.lines")
      junk-file                                 (etc-path "junk")
      initial-scratch-message                   (yow-comment)
      initial-major-mode                        'emacs-lisp-mode
      line-number-mode                          t
      column-number-mode                        t
      size-indication-mode                      t
      font-lock-maximum-decoration              t
      require-final-newline                     t
      echo-keystrokes                           0.1
      transient-mark-mode                       t
      color-theme-is-global                     t
      shift-select-mode                         nil
      require-final-newline                     t
      truncate-partial-width-windows            nil
      x-select-enable-clipboard                 t
      pager-increment                           5
      help-window-select                        nil
      visible-cursor                            nil
      ;; idle-update-delay                      0.1
      )

;; buffer-local defaults

(set-default 'indent-tabs-mode      nil)
(set-default 'indicate-empty-lines  nil)

;; mode toggles

(global-font-lock-mode     t)
(global-auto-revert-mode   1)
(auto-compression-mode     t)
(auto-image-file-mode      t)
(show-paren-mode          -1)
(delete-selection-mode    -1)

;; info path

(add-to-list 'Info-default-directory-list (etc-path "info"))

;; default-frame-alist

;; (set-frame-font "Menlo-12")
;; (set-frame-font "DejaVu Sans Mono-12")

(setq default-frame-alist
      `((font                 . "Menlo-12")
        ;; (font                 . "DejaVu Sans Mono-12")
        (vertical-scroll-bars . nil)
        (tool-bar-lines       . 0)
        (menu-bar-lines       . 0)
        (background-mode      . dark)
        (background-color     . "Grey15")
        (border-color         . "Black")
        ;; (cursor-color         . "Grey")
        (cursor-color         . "medium slate blue")
        (foreground-color     . "Grey")
        (mouse-color          . "Grey")
        ))

;; enable commands

(mapc 'enable-command '(set-goal-column scroll-left))

;; seed random

(random t)

;; provide

(provide 'tlh-init)

;;; tlh-init.el ends here
