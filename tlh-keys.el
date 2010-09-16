;;; defkeymaps

(defkeymap buffer-map
  "C-e"              'eval-buffer
  "C-r"              'revert-buffer
  "C-c"              'cleanup-buffer
  "C-u"              'untabify-buffer
  "C-b"              'bury-buffer
  "C-z"              'inhibit-backup-of-buffer
  )


(defkeymap file-map
  "C-c"              'file-cache-minibuffer-complete
  "C-l"              'load-file
  "C-m"              'move-file-and-buffer
  "C-o"              'ido-find-file-other-window
  "C-r"              'ido-recentf-find-file
  "C-s"              'ido-sudo-find-file
  )


(defkeymap directory-map
  "C-r"              'byte-recompile-directory
  "C-m"              'make-directory
  )


(defkeymap workgroups-map
  "b"                'workgroups-ido-switch
  "C-a"              'workgroups-ido-add
  "C-b"              'workgroups-ido-switch
  "C-e"              'workgroups-show-current
  "C-f"              'workgroups-find-file
  "C-j"              'workgroups-bury
  "C-k"              (cmd (workgroups-kill))
  "C-n"              'workgroups-next
  "C-p"              'workgroups-previous
  "C-r"              'workgroups-revert
  "C-s"              'workgroups-save
  "C-u"              'workgroups-update
  "C-v"              'workgroups-random
  "C-z"              'workgroups-ido-raise
  "S-C-a"            'workgroups-add
  "S-C-d"            'workgroups-kill
  "S-C-r"            'workgroups-switch
  )


(defkeymap region-map
  "C-k"              'kill-whole-paragraph
  "C-s"              'save-paragraph
  "C-u"              'uncomment-paragraph
  "C-b"              'comment-or-uncomment-paragraph
  "C-d"              'comment-defun
  "C-c"              'duplicate-paragraph
  "C-x"              'duplicate-and-comment-paragraph
  "C-j"              (cmd (kill-paragraph-append-to-file junk-file))
  "C-i"              'indent-paragraph
  )


(defkeymap emms-map
  "C-SPC"            'emms-pause
  "C-b"              'emms-browser
  "C-d"              'emms-play-directory
  "C-f"              'emms-play-file
  "C-q"              'emms-stop
  "C-l"              'emms-playlist-mode-go
  "C-m"              'emms-mode-line-toggle
  "C-r"              'emms-shuffle
  "C-s"              'emms-show
  "C-t"              'emms-play-directory-tree
  "C-x"              'emms-play-find
  "C-p C-n"          'emms-playlist-new
  "C-p C-s"          'emms-playlist-save
  "C-p C-l"          'emms-playlist-mode-load-playlist
  )


(defkeymap shell-map
  "C-a"              'ansi-term
  "C-e"              'eshell
  "C-h"              'shell
  "C-i"              'ielm
  "C-s"              'slime-connect
  "C-t"              'term
  )


(defkeymap mode-map
  "C-a"              'auto-fill-mode
  "C-c"              'clojure-mode
  "C-d"              'eldoc-mode
  "C-e"              'emacs-lisp-mode
  "C-f"              'fundamental-mode
  "C-g"              'workgroups-mode
  "C-i"              'lisp-interaction-mode
  "C-k"              'markdown-mode
  "C-l"              'lisp-mode
  "C-m"              'magit-status
  "C-o"              'org-mode
  "C-p"              'paredit-mode
  "C-P"              'show-paren-mode
  "C-r"              'rainbow-mode
  "C-s"              'recs-mode
  "C-t"              'text-mode
  "C-u"              'toggle-truncate-lines
  "C-w"              'whitespace-mode
  "C-y"              'flyspell-mode
  "C-z"              'zone
  )


(defkeymap launch-map
  "C-g"              'gnus
  "C-w"              'w3m
  )


(defkeymap games-map
  "C-d"              'doctor
  "C-l"              'life
  "C-p"              'pong
  "C-s"              'solitaire
  "C-t"              'tetris
  "C-u"              'dunnet
  )


(defkeymap erc-map
  "C-b"              'switch-to-erc-buffer
  "C-S-b"            'erc-iswitchb
  "C-c"              'erc-freenode-ssl-connect
  "C-n"              'next-erc-buffer
  "C-p"              'prev-erc-buffer
  )


(defkeymap browse-map
  "C-b"              'browse-url-at-point
  "C-e"              'google-emacswiki-search
  "C-l"              'google-lucky-search
  "C-n"              'browse-next-url
  "C-o"              'google-search
  "C-p"              'browse-previous-url
  "C-w"              'google-wikipedia-search
  "C-z"              (cmd (browse-url "http://www.zombo.com"))
  )


(defkeymap w3m-map
  "C-l"              'w3m-lucky-search-new-session
  "C-o"              'w3m-view-this-url-new-session
  "C-s"              'w3m-search-new-session
  "C-u"              'w3m-goto-url-new-session
  "C-w"              'w3m-emacswiki-new-session
  )


(defkeymap org-map
  "C-a"              'org-agenda
  "C-l"              'org-store-link
  )


(defkeymap acct-map
  "C-a"              'acctdb-display-accounts
  "C-c"              'acctdb-create-entry
  )


(defkeymap yaoddmuse-map
  "C-d"              'yaoddmuse-edit-default
  )


(defkeymap command-root-map
  "C-a"              acct-map
  "C-b"              buffer-map
  "C-c"              region-map
  "C-d"              directory-map
  "C-f"              file-map
  "C-g"              games-map
  "C-h"              help-map
  "C-i"              erc-map
  "C-j"              browse-map
  "C-m"              mode-map
  "C-n"              shell-map
  "C-o"              org-map
  "C-p"              launch-map
  "C-u"              w3m-map
  "C-v"              emms-map
  "C-x"              workgroups-map
  "C-y"              yaoddmuse-map
  )


;;; fill-keymaps

(fill-keymap dired-mode-map
             "C-c w"            'wdired-change-to-wdired-mode
             )


(fill-keymap text-mode-map
             "C-M-q"            'unfill-paragraph
             )


(fill-keymap isearch-mode-map
             "M-h"              isearch-help-map
             "C-h"              'isearch-delete-char
             )


(fill-keymap lisp-mode-shared-map
             "M-."              'find-function-at-point
             "C-c C-e"          'eval-last-sexp
             "C-c e"            'eval-and-replace
             "C-c l"            "lambda"
             "C-\\"             'lisp-complete-symbol
             "C-c C-j"          'list-indent
             "C-S-h"            'mark-list
             )


(fill-keymap help-map
             "f"                'ido-describe-function
             "C-f"              'ido-describe-function-at-point
             "v"                'ido-describe-variable
             "C-v"              'ido-describe-variable-at-point
             "V"                'apropos-variable
             "c"                'ido-find-documentation-for-command
             "C-k"              'describe-key-briefly
             "a"                'apropos
             "C-a"              'apropos-command
             "A"                'about-emacs
             "z"                'apropos-zippy
             "C-l"              'apropos-library
             "C-i"              'info-apropos
             )


(fill-keymap ctl-x-map
             "p"                'message-point
             "C-u"              'untabify
             "\\"               'align-regexp
             ;; "C-c"              'execute-extended-command
             "C-m"              'ido-execute-extended-command
             "C-i"              'ido-imenu
             "7"                'ido-recalculate-all-caches
             "C-b"              'ibuffer
             "u"                'yell-at-me
             "C-e"              'yell-at-me
             "("                'yell-at-me
             ")"                'yell-at-me
             ;; "C-c"              'save-buffers-kill-terminal
             )


(fill-keymap mode-specific-map
             "]"                'close-all-parens
             "s"                'slime-selector
             "C-s C-s"          (cmd (slime-connect "localhost" 4005))
             )


(fill-keymap global-map

             "C-z"              command-root-map
             "C-h"              'backward-delete-char-untabify
             "M-w"              'kill-ring-save
             "C-w"              'backward-kill-word
             ;; "s-b"              'ido-switch-buffer
             "H-b"              'ido-switch-buffer
             "C-M-."            'ido-find-function
             "C-M-o"            'ffap
             "C-M-w"            'kill-region
             "M-/"              'hippie-expand
             "C-s"              'isearch-forward-regexp
             "C-r"              'isearch-backward-regexp
             "C-M-s"            'isearch-forward
             "C-M-r"            'isearch-backward
             "C-+"              'text-scale-increase
             "C--"              'text-scale-decrease
             "C-S-k"            'save-line
             "M-RET"            'ns-toggle-fullscreen
             "<backspace>"      'yell-at-me
             "M-x"              'yell-at-me
             "M-DEL"            'yell-at-me
             "C-_"              'yell-at-me
             "M-_"              (cmd (dec-transparency 2))
             "M-+"              (cmd (inc-transparency 2))
             "C-x C-\\"         'goto-last-change


             ;; Hyper

             "H-k"              'kill-whole-line


             ;; quickkeys

             "C-M-1"            'get-scratch-buffer
             "C-M-2"            'eshell
             "C-M-3"            'jump-to-register
             "C-M-4"            (cmd (switch-to-buffer (help-buffer)))
             "C-M-5"            'google-define
             "C-M-7"            'recs-load-pattern-file
             "C-M-8"            'refresh-frame
             "C-M-9"            (cmd (checkdoc-current-buffer t))


             ;; buffers

             "C-H-h"            'previous-buffer
             "C-H-l"            'next-buffer
             "C-H-j"            'bury-buffer
             "C-H-k"            (cmd (kill-buffer (current-buffer)))


             ;; windows

             "C-H-f"            'windmove-right
             "C-H-b"            'windmove-left
             "C-H-n"            'windmove-down
             "C-H-p"            'windmove-up
             "C-H-e"            (cmd (other-window  1))
             "C-H-a"            (cmd (other-window -1))
             "C-H-0"            'delete-window
             "C-H-1"            'delete-other-windows
             "C-H-2"            'split-window-vertically
             "C-H-3"            'split-window-horizontally
             "C-H-4"            'window-configuration-to-register
             "C-H-o"            'scroll-left
             "C-H-y"            'scroll-right
             "C-H-u"            'scroll-up
             "C-H-i"            'scroll-down
             "C-H-S-n"          'inc-window-height
             "C-H-S-p"          'dec-window-height
             "C-H-S-f"          'inc-window-width
             "C-H-S-b"          'dec-window-width
             "C-H-<up>"         'inc-window-height
             "C-H-<down>"       'dec-window-height
             "C-H-<right>"      'inc-window-width
             "C-H-<left>"       'dec-window-width


             ;; frames

             "C-M-S-h"          (cmd (other-frame -1))
             "C-M-S-l"          (cmd (other-frame  1))
             "C-M-S-k"          'delete-frame
             "C-M-S-m"          'make-frame
             "C-M-S-r"          'refresh-frame


             ;; workgroups

             "C-H-s"            'workgroups-update
             "C-H-,"            'workgroups-previous
             "C-H-."            'workgroups-next


             ;; moving point

             "H-n"              'pager-down
             "H-p"              'pager-up
             "H-N"              (cmd (pager-down 5))
             "H-P"              (cmd (pager-up   5))


             ;; transposing

             "C-S-t"            'backward-transpose-chars
             "M-T"              'backward-transpose-words
             ;; "s-t"              'transpose-lines
             ;; "s-T"              'backward-transpose-lines
             "H-t"              'transpose-lines
             "H-T"              'backward-transpose-lines
             "C-M-S-t"          'backward-transpose-sexps
             "M-p"              'transpose-paragraphs
             "M-P"              'backward-transpose-paragraphs


             ;; fn keys

             "<f1>"             'info-apropos
             "<f2>"             'man
             "<f5>"             'slime-connect
             "<f7>"             'emms-previous
             "<f8>"             'emms-pause
             "<f9>"             'emms-next
             "<f10>"            'toggle-mute-volume
             "<f11>"            'decrease-volume
             "<f12>"            'increase-volume
             "S-<f7>"           'emms-slider-seek-backward
             "S-<f8>"           'emms-stop
             "S-<f9>"           'emms-slider-seek-forward
             "S-<f10>"          'show-volume
             "S-<f11>"          'emms-mplayer-decrease-volume
             "S-<f12>"          'emms-mplayer-increase-volume
             "C-<f7>"           (cmd (emms-slider-seek-backward 30))
             "C-<f9>"           (cmd (emms-slider-seek-forward  30))
             "M-<f1>"           'totd
             "M-<f5>"           'ucs-insert
             "M-<f7>"           'emms-slider-seek
             "M-<f9>"           'emms-slider-seek
             "M-<f12>"          'current-info
             "C-M-<f7>"         'emms-display-track-position-slider
             "C-M-<f9>"         'emms-display-track-position-slider
             "C-M-<f1>"         'view-emacs-FAQ
             "C-M-<f2>"         'describe-copying
             "C-M-<f3>"         'fortune


             ;; erc

             "C-H-["            'prev-erc-buffer
             "C-H-]"            'next-erc-buffer

             )


;;; provide

(provide 'tlh-keys)


;;; tlh-keys.el ends here
