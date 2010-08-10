;; defkeymaps

(defkeymap buffer-map
  "C-e"              'eval-buffer
  "C-r"              'revert-buffer
  "C-c"              'cleanup-buffer
  "C-u"              'untabify-buffer
  "C-b"              'bury-buffer
  "C-z"              'inhibit-backup-of-buffer
  )

(defkeymap file-map
  "C-r"              'ido-recentf-find-file
  "C-o"              'ido-find-file-other-window
  "C-s"              'ido-sudo-find-file
  "C-c"              'file-cache-minibuffer-complete
  "C-l"              'load-file
  )

(defkeymap directory-map
  "C-r"              'byte-recompile-directory
  "C-m"              'make-directory
  )

(defkeymap workgroups-map
  "C-a"              'workgroups-ido-add-config
  "C-e"              'workgroups-echo-current-config
  "C-r"              'workgroups-ido-restore-config
  "C-d"              'workgroups-ido-delete-config
  "S-C-a"            'workgroups-add-config
  "S-C-r"            'workgroups-restore-config
  "S-C-d"            'workgroups-delete-config
  "C-u"              'workgroups-update-config
  "C-v"              'workgroups-revert-config
  "C-l"              'workgroups-load-configs
  "C-p"              'workgroups-prev-config
  "C-n"              'workgroups-next-config
  )

(defkeymap block-map
  "C-k"              'kill-block
  "C-s"              'save-block
  "C-u"              'uncomment-block
  "C-b"              'comment-or-uncomment-block
  "C-d"              'comment-defun
  "C-c"              'duplicate-block
  "C-x"              'duplicate-and-comment-block
  "C-j"              (cmd (kill-block-append-to-file junk-file))
  "C-i"              'indent-block
  )

(defkeymap emms-map
  "C-SPC"            'emms-start
  "C-r"              'emms-shuffle
  "C-s"              'emms-show
  "C-f"              'emms-play-file
  "C-d"              'emms-play-directory
  "C-t"              'emms-play-directory-tree
  "C-l"              'emms-playlist-mode-go
  "C-x"              'emms-play-find
  "C-p C-n"          'emms-playlist-new
  "C-p C-s"          'emms-playlist-save
  "C-p C-l"          'emms-playlist-mode-load-playlist
  )

(defkeymap shell-map
  "C-e"              'eshell
  "C-s"              'shell
  "C-t"              'term
  "C-a"              'ansi-term
  "C-i"              'ielm
  )

(defkeymap mode-map
  "C-a"              'auto-fill-mode
  "C-c"              'clojure-mode
  "C-e"              'emacs-lisp-mode
  "C-f"              'fundamental-mode
  "C-k"              'markdown-mode
  "C-l"              'lisp-mode
  "C-m"              'magit-status
  "C-o"              'org-mode
  "C-p"              'paredit-mode
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
  "C-p"              'browse-prev-url
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
  "C-c"              block-map
  "C-d"              directory-map
  "C-f"              file-map
  "C-g"              games-map
  "C-h"              help-map
  "C-i"              erc-map
  "C-j"              browse-map
  "C-m"              mode-map
  "C-o"              org-map
  "C-p"              launch-map
  "C-s"              shell-map
  "C-u"              w3m-map
  "C-v"              emms-map
  "C-x"              workgroups-map
  "C-y"              yaoddmuse-map
  )

;; fill-keymaps

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
             "s"                'slime-selector
             "C-s C-s"          (cmd (slime-connect "localhost" 4005))
             )

(fill-keymap global-map

             "C-z"              command-root-map
             "C-h"              'backward-delete-char-untabify
             "M-w"              'kill-ring-save
             "C-w"              'backward-kill-word
             "s-b"              'ido-switch-buffer
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
             "S-C-k"            'kill-ring-save-line
             "M-RET"            'ns-toggle-fullscreen
             "<backspace>"      'yell-at-me
             "M-x"              'yell-at-me
             "M-DEL"            'yell-at-me
             "C-_"              'yell-at-me
             "M-_"              (cmd (dec-transparency 2))
             "M-+"              (cmd (inc-transparency 2))

             ;; quickkeys

             "C-M-1"            'get-scratch-buffer
             "C-M-2"            'eshell
             "C-M-3"            'jump-to-register
             "C-M-4"            (cmd (switch-to-buffer (help-buffer)))
             "C-M-5"            'google-define
             "C-M-9"            (cmd (checkdoc-current-buffer t))

             ;; buffers

             "C-s-h"            'previous-buffer
             "C-s-l"            'next-buffer
             "C-s-j"            'bury-buffer
             "C-s-k"            (cmd (kill-buffer (current-buffer)))

             ;; windows

             "C-s-f"            'windmove-right
             "C-s-b"            'windmove-left
             "C-s-n"            'windmove-down
             "C-s-p"            'windmove-up
             "C-s-e"            (cmd (other-window  1))
             "C-s-a"            (cmd (other-window -1))
             "C-s-0"            'delete-window
             "C-s-1"            'delete-other-windows
             "C-s-2"            'split-window-vertically
             "C-s-3"            'split-window-horizontally
             "C-s-4"            'window-configuration-to-register
             "C-s-o"            'scroll-left
             "C-s-y"            'scroll-right
             "C-s-u"            'scroll-up
             "C-s-i"            'scroll-down
             "C-S-s-n"          'inc-window-height
             "C-S-s-p"          'dec-window-height
             "C-S-s-f"          'inc-window-width
             "C-S-s-b"          'dec-window-width
             "C-s-<up>"         'inc-window-height
             "C-s-<down>"       'dec-window-height
             "C-s-<right>"      'inc-window-width
             "C-s-<left>"       'dec-window-width

             ;; workgroups

             "C-s-s"            'workgroups-update-config
             "C-s-,"            'workgroups-prev-config
             "C-s-."            'workgroups-next-config

             ;; moving point

             "s-n"              'pager-down
             "s-p"              'pager-up
             "s-N"              (cmd (pager-down pager-increment))
             "s-P"              (cmd (pager-up   pager-increment))

             ;; transposing

             "C-S-t"            'backward-transpose-chars
             "M-T"              'backward-transpose-words
             "s-t"              'transpose-lines
             "s-T"              'backward-transpose-lines
             "C-M-S-t"          'backward-transpose-sexps
             "M-p"              'transpose-paragraphs
             "M-P"              'backward-transpose-paragraphs

             ;; fn keys

             "<f1>"             'info-apropos
             "<f2>"             'man
             "<f10>"            'mute-volume
             "<f11>"            'decrease-volume
             "<f12>"            'increase-volume
             "M-<f1>"           'totd
             "M-<f5>"           'ucs-insert
             "M-<f12>"          'current-info
             "C-M-<f1>"         'view-emacs-FAQ
             "C-M-<f2>"         'describe-copying
             "C-M-<f3>"         'fortune

             ;; emms

             "<f7>"             'emms-previous
             "<f8>"             'emms-pause
             "<f9>"             'emms-next
             "S-<f7>"           'emms-seek-backward
             "S-<f9>"           'emms-seek-forward

             ;; erc

             "C-s-["            'prev-erc-buffer
             "C-s-]"            'next-erc-buffer

             )

(provide 'tlh-keys)
