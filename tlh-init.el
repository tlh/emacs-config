;;; core configs


;;; server

(require 'server)
(or (server-running-p) (server-start))


;;; coding system

;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
(prefer-coding-system       'utf-8)


;;; set variables

(setq
 user-full-name                            "tlh"
 user-mail-address                         "thunkout@gmail.com"
 user-emacs-directory                      (emacs-path)
 backup-directory-alist                   `(("." . ,(home-path "tcvol1/emacs-misc/backups/")))
 custom-file                               (elisp-path "tlh-custom.el")
 auto-save-list-file-prefix                (etc-path "auto-save-list/.saves-")
 tex-directory                             (etc-path "tex")
 yow-file                                  (etc-path "yow.lines")
 junk-file                                 (etc-path "junk")
 initial-major-mode                        'emacs-lisp-mode
 inhibit-startup-echo-area-message         user-login-name
 initial-scratch-message                   (yow-comment)
 inhibit-splash-screen                     t
 inhibit-startup-message                   t
 enable-recursive-minibuffers              t
 line-number-mode                          t
 column-number-mode                        t
 size-indication-mode                      t
 font-lock-maximum-decoration              t
 require-final-newline                     t
 transient-mark-mode                       t
 color-theme-is-global                     t
 require-final-newline                     t
 x-select-enable-clipboard                 t
 scroll-preserve-screen-position           t
 undo-limit                                1000000
 undo-outer-limit                          nil
 undo-string-limit                         undo-limit
 history-length                            10000
 read-quoted-char-radix                    10
 next-screen-context-lines                 1
 scroll-margin                             0
 ;; scroll-up-aggressively                    0
 ;; scroll-down-aggressively                  0
 confirm-nonexistent-file-or-buffer        nil
 redisplay-dont-pause                      nil
 redisplay-preemption-period               0.1
 scroll-conservatively                     10000
 echo-keystrokes                           0.1
 ;; idle-update-delay                      0.1
 truncate-partial-width-windows            nil
 shift-select-mode                         nil
 help-window-select                        nil
 visible-cursor                            nil
 tooltip-use-echo-area                     nil
 )


;;; set buffer-local variable defaults

(setq-default
 fill-column                               80
 indent-tabs-mode                          nil
 indicate-empty-lines                      nil
 cursor-type                              'box)


;;; turn on some modes

(global-font-lock-mode     t)
(global-auto-revert-mode   1)
(auto-compression-mode     t)
(auto-image-file-mode      t)
(delete-selection-mode    -1)
(blink-cursor-mode         nil)
(tooltip-mode              nil)


;;; info path

(add-to-list 'Info-default-directory-list (etc-path "info"))


;;; default-frame-alist

(setq default-frame-alist
      `((font                      . "Menlo-12") ;; "DejaVu Sans Mono-12"
        (background-mode           . dark)
        (background-color          . "Grey15")
        (border-color              . "Black")
        ;; (cursor-color           . "Grey")
        (cursor-color              . "medium slate blue")
        (foreground-color          . "Grey")
        (mouse-color               . "Grey")
        (vertical-scroll-bars      . nil)
        (horizontal-scroll-bars    . nil)
        (tool-bar-lines            . 0)
        (menu-bar-lines            . 0)
        (right-fringe              . 4)
        (left-fringe               . 4)
        (internal-border-width     . 2)
        ;; (border-width              . 0)
        ))


;;; enable disabled commands

(mapc 'command-enable '(set-goal-column
                        scroll-left
                        narrow-to-region
                        erase-buffer))


;;; seed random

(random t)


;;; provide

(provide 'tlh-init)


;;; tlh-init.el ends here
