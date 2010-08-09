(eval-when-compile
  (require 'cl)
  (require 'server))

(or (server-running-p) (server-start))

(setq inhibit-splash-screen            t
      inhibit-startup-message          t
      user-full-name                   "tlh"
      user-mail-address                "thunkout@gmail.com"
      custom-file                      (elisp-path "tlh-custom.el")
      auto-save-list-file-prefix       (etc-path "auto-save-list/.saves-")
      backup-directory-alist          `(("." . ,(home-path "tcvol1/emacs-misc/backups/")))
      yow-file                         (etc-path "yow.lines")
      junk-file                        (etc-path "junk")
      initial-scratch-message          (yow-comment)
      line-number-mode                 t
      column-number-mode               t
      size-indication-mode             t
      font-lock-maximum-decoration     t
      require-final-newline            t
      echo-keystrokes                  0.1
      transient-mark-mode              t
      color-theme-is-global            t
      shift-select-mode                nil
      require-final-newline            t
      truncate-partial-width-windows   nil
      x-select-enable-clipboard        t
      pager-increment                  5
      help-window-select               nil
      )

(add-to-list 'Info-default-directory-list (etc-path "info"))
(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines nil)
(global-font-lock-mode t)
(global-auto-revert-mode 1)
(auto-compression-mode t)
(auto-image-file-mode t)
(show-paren-mode -1)
(random t)

;; enable commands

(mapc 'enable-command '(set-goal-column scroll-left))

;; coding system

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system       'utf-8)

;; gui specific

(when window-system
  (menu-bar-mode      -1)
  (tool-bar-mode      -1)
  (scroll-bar-mode    -1)
  (tooltip-mode       -1)
  (blink-cursor-mode  -1)
  (mouse-wheel-mode    t))

;; system specific

(case system-type
  ((gnu/linux linux)
   nil)
  (darwin
   (require 'tlh-mac)))

;; ding

(defvar quiet-functions '(isearch-abort
                          abort-recrusive-edit
                          exit-minibuffer
                          keyboard-quit))

(defun pretty-ding ()
  (unless (memq this-command quiet-functions)
    (play-sound ding-sound)))

(setq ring-bell-function 'pretty-ding)

(defun yell-at-me (&optional msg)
  (interactive)
  (play-sound yell-sound)
  (message (or msg (yow))))

(provide 'tlh-init)
