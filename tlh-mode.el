;;; mode configs


;;; tlh-ido

(require 'tlh-ido)


;;; tlh-emms

(require 'tlh-emms)


;;; tlh-erc

(require 'tlh-erc)


;;; tlh-sound

(require 'tlh-sound)

(setq ring-bell-function 'pretty-ding
      volume-increment    2)

(add-all-to-list 'quiet-functions
                 'isearch-abort
                 'abort-recrusive-edit
                 'exit-minibuffer
                 'keyboard-quit)


;;; tlh-notify

(require 'tlh-notify)


;;; workgroups

(global-unset-key (kbd "C-z"))

(add-path (elisp-path "workgroups"))

(require 'workgroups)

(setq
 ;; wg-prefix-key             (kbd "C-z C-x")
 wg-prefix-key             (kbd "C-c w")
 wg-frame-morph-hsteps     9
 wg-frame-morph-vsteps     3
 )

(workgroups-mode t)

;; (and (eq window-system 'ns)
;;      (fboundp `ns-toggle-fullscreen)
;;      (ns-toggle-fullscreen))

(wg-load (etc-path "workgroups-configs"))


;;; color-theme

(when window-system
  (add-path (site-path "color-theme"))
  (setq color-theme-load-all-themes nil)
  (require 'color-theme)
  (add-path (elisp-path "color-themes/"))
  (require 'color-theme-thunk1)
  (add-hook 'after-init-hook 'color-theme-thunk1))


;;; org

(setq org-hide-leading-stars t)


;;; show-paren-mode

(setq show-paren-delay                  0.0
      show-paren-ring-bell-on-mismatch  t
      show-paren-style                  'parenthesis)

(show-paren-mode 1)


;;; bbdb

(add-path (site-path "bbdb"))
(require 'bbdb)
(bbdb-initialize)


;;; comint

(setq-default comint-scroll-show-maximum-output nil
              comint-scroll-to-bottom-on-input  nil)

(setq comint-prompt-read-only t)


;;; recentf

(recentf-mode 1)
(setq recentf-save-file (etc-path "recentf"))


;;; saveplace

(require 'saveplace)
(setq save-place-file (etc-path "saved-places"))


;;; whitespace-mode

(setq whitespace-line-column 100
      whitespace-style '(trailing
                         space-before-tab
                         indentation
                         space-after-tab))


;;; text-mode

(add-hook 'text-mode-hook 'turn-on-auto-fill)


;;; coding-hook

(defun coding-hook ()
  (setq save-place t)
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t)
  (whitespace-mode t)
  (add-watchwords))


;;; emacs-lisp-mode

(setq eval-expression-print-level   20
      eval-expression-print-length  20)

(add-hooks 'emacs-lisp-mode-hook
           'turn-on-eldoc-mode
           ;; 'remove-elc-on-save
           'coding-hook
           'unicode-lambdas
           'cleanup-buffer-on-save)


;;; lisp-mode

(add-hooks 'lisp-mode-hook
           'coding-hook
           'unicode-lambdas
           'cleanup-buffer-on-save)


;;; windmove

(require 'windmove)
(setq windmove-wrap-around t)


;; ;;; framemove
;; (add-path (site-path "framemove"))
;; (require 'framemove)
;; (setq framemove-hook-into-windmove t)


;;; browse-kill-ring

(add-path (site-path "browse-kill-ring"))
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(setq browse-kill-ring-quit-action 'save-and-restore)


;;; imenu

(require 'imenu)
(setq imenu-auto-rescan t)


;;; dired

(require 'dired)

(setq directory-free-space-program        "df"
      directory-free-space-args           "-h"
      dired-auto-revert-buffer            t
      wdired-allow-to-change-permissions  t)

(fill-keymap dired-mode-map "E" 'emms-play-dired)

(command-enable 'dired-find-alternate-file)


;;; eshell

(defpathfn eshell-path (etc-path "eshell/"))

(defun tlh-eshell-prompt-fn ()
  (format "%s%s "
          (abbreviate-file-name (eshell/pwd))
          (if (= (user-uid) 0) "#" "$")))

(defun tlh-setup-eshell ()
  (require 'em-prompt)
  (require 'em-term)
  (require 'em-cmpl)
  (require 'em-banner)

  (add-all-to-list 'eshell-visual-commands "ssh" "tail")
  (add-all-to-list 'eshell-command-completions-alist
                   '("gunzip" . "gz\\'")
                   '("tar" . "\\(\\.tar\\|\\.tgz\\|\\.tar\\.gz\\)\\'"))

  (setq eshell-directory-name              (eshell-path)
        eshell-aliases-file                (eshell-path "alias")
        eshell-history-file-name           (eshell-path "history")
        eshell-last-dir-ring-file-name     (eshell-path "lastdir")
        eshell-ls-use-in-dired             t
        eshell-save-history-on-exit        t
        eshell-cmpl-cycle-completions      t
        eshell-scroll-show-maximum-output  nil
        eshell-scroll-to-bottom-on-output  nil
        eshell-prompt-function            'tlh-eshell-prompt-fn
        eshell-prompt-regexp               "^[^#$\n]*[#$] "
        eshell-banner-message              " -={ Fuck Off }=-\n\n"))

(add-hook 'eshell-load-hook 'tlh-setup-eshell)


;;; recs-mode

(add-path (elisp-path "recs-mode"))
(require 'recs-mode)
(recs-mode t)
(setq recs-suggestion-interval   nil
      recs-ding-on-suggestion    nil
      recs-suggestion-window     t
      recs-suppress-suggestion   nil
      recs-window-select         t
      recs-log-suggestions       t
      recs-log-file              (etc-path "recs-log"))

(add-hook 'recs-mode-hook 'yell-at-me)


;;; uniquify

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator "|"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")


;;; epa

(when (require 'epa-file nil t)
  (setenv "GPG_AGENT_INFO"))


;;; pickel

(add-path (elisp-path "pickel"))
(require 'pickel)


;;; kvdb

(add-path (elisp-path "kvdb"))
(require 'kvdb)
(setq kvdb-completing-read-fn 'ido-completing-read)
(kvdb-mode t)
(kvdb-load "~/tcvol1/.accts")


;;; tls

(require 'tls)
(setq tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof"))


;;; tramp

(require 'tramp)
(setq tramp-persistency-file-name (etc-path "tramp/tramp-persistence")
      tramp-auto-save-directory (etc-path "tramp/"))


;;; gnus

(setq gnus-init-file (elisp-path "tlh-gnus.el"))
;; (setq gnus-startup-file (elisp-path "newsrc.el"))


;;; midnight-mode

(require 'midnight)
(midnight-delay-set 'midnight-delay "7:00AM")


;;; ansi-term

(setq ansi-term-color-vector [unspecified "black" "red3" "lime green" "yellow3"
                                          "DeepSkyBlue3" "magenta3" "cyan3" "white"]
      term-default-fg-color  "Grey"
      term-default-bg-color  "Grey15")


;;; paredit

(add-path (site-path "paredit"))
(autoload 'paredit-mode "paredit" nil t)

;; (add-hook 'lisp-mode-hook (lambda () (paredit-mode t)))
;; (add-hook 'clojure-mode-hook (lambda () (paredit-mode t)))


;;; js2-mode

(add-path (site-path "js2-mode"))
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?$" . js2-mode))


;;; slime

(add-path (site-path "slime"))

(require 'slime-autoloads)
(require 'slime)

(slime-setup '(slime-asdf
               slime-banner
               slime-repl
               ;; slime-autodoc
               slime-editing-commands
               slime-fancy-inspector
               slime-fuzzy
               slime-presentations
               slime-scratch
               slime-references
               slime-package-fu
               slime-fontifying-fu
               ))

(setq slime-protocol-version 'ignore
      slime-lisp-implementations
      `((sbcl ("/usr/local/bin/sbcl" "--core" ,(etc-path "sbcl.core-for-slime")))
        (clozure ("/usr/bin/dx86cl64"))))

(defun define-slime-keys ()
  (fill-keymap slime-mode-map
               "C-M-:"          'slime-interactive-eval
               "C-c C-e"        'slime-eval-last-expression
               "C-z C-b C-e"    'slime-eval-buffer))

(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'slime-mode-hook 'define-slime-keys)

;; Override slime-repl-show-maximum-output

(fset 'original-slime-repl-show-maximum-output
      (symbol-function 'slime-repl-show-maximum-output))

(defvar slime-repl-show-maximum-output nil)

(defun slime-repl-show-maximum-output ()
  (when slime-repl-show-maximum-output
    (original-slime-repl-show-maximum-output)))


;;; clojure-mode

(add-path (site-path "clojure-mode"))
;; (autoload 'clojure-mode "clojure-mode" nil t)
(require 'clojure-mode)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-hook 'clojure-mode-hook
          'coding-hook
          'unicode-lambdas
          'cleanup-buffer-on-save)

;; conditionally setup slime repl for clojure

(defun slime-clojure-repl-setup ()
  (when (string-equal "clojure" (slime-connection-name))
    (clojure-mode-font-lock-setup)
    (set-syntax-table clojure-mode-syntax-table)
    (setq lisp-indent-function 'clojure-indent-function)
    (when (and (featurep 'paredit) paredit-mode (>= paredit-version 21))
      (define-key slime-repl-mode-map "{" 'paredit-open-curly)
      (define-key slime-repl-mode-map "}" 'paredit-close-curly))))

(add-hook 'slime-repl-mode-hook 'slime-clojure-repl-setup)


;;; markdown-mode

(add-path (site-path "markdown-mode"))
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\|\\.mdwn\\|\\.mdt" . markdown-mode))


;;; magit

(add-path (site-path "magit"))
(autoload 'magit-status "magit" nil t)


;;; jd-el

(add-path (site-path "jd-el"))
(autoload 'rainbow-mode "rainbow-mode" nil t)
(autoload 'google-maps "google-maps" nil t)


;;; url vars

(defvar strict-url-regexp "https?://")
(defvar permissive-url-regexp "\\(https?://\\)?\\(www\\.\\)?.+\\....?")
(defvar google-search-string "http://www.google.com/search?q=")
(defvar google-lucky-search-string "http://www.google.com/search?btnI=I'm+Feeling+Lucky&q=")


;;; w3m

(add-path (site-path "w3m"))

(require 'w3m-load)
;; (require 'mime-w3m)

(setq w3m-key-binding            'info
      w3m-home-page              "about:"
      w3m-default-directory      (etc-path "w3m/")
      w3m-profile-directory      (etc-path "w3m/")
      w3m-fill-column            100)

(defun w3m-emacswiki-new-session (topic)
  (interactive "sTopic: ")
  (w3m-goto-url-new-session
   (concat google-lucky-search-string "site:emacswiki.org+" topic)))


;;; browse-url

(defun tlh-encode-url (url)
  (browse-url-url-encode-chars url "[,)$]\")."))

(defun tlh-google-search (lucky &rest terms)
  (let ((search (if lucky google-lucky-search-string google-search-string)))
    (browse-url (tlh-encode-url (concat search (mapconcat 'identity terms " "))))))

(defun browse-url-find (finder-fn &optional num)
  (save-excursion
    (dotimes (i (or num 1))
      (goto-char (funcall finder-fn)))
    (browse-url-at-point)))

(defun browse-previous-url (&optional num)
  (interactive "P")
  (browse-url-find (lambda () (search-backward-regexp strict-url-regexp)) num))

(defun browse-next-url (&optional num)
  (interactive "P")
  (browse-url-find (lambda () (search-forward-regexp strict-url-regexp)) num))

(defun google-search (terms)
  (interactive (list (if (region-active-p)
                         (buffer-substring (region-beginning) (region-end))
                       (read-from-minibuffer "Query: " (thing-at-point 'sexp)))))
  (tlh-google-search nil terms))

(defun google-lucky-search (terms)
  (interactive (mbq-symbol-at-point "Lucky search: "))
  (tlh-google-search t terms))

(defun google-wikipedia-search (terms)
  (interactive (mbq-symbol-at-point "Wikipedia search: "))
  (tlh-google-search t "site:wikipedia.org" terms))

(defun google-emacswiki-search (terms)
  (interactive (mbq-symbol-at-point "Emacswiki search: "))
  (tlh-google-search t "site:emacswiki.org" terms))


;;; google-define

(add-path (site-path "google-define"))
(require 'google-define)


;;; yaoddmuse

(add-path (site-path "yaoddmuse"))
(require 'yaoddmuse)
(setq yaoddmuse-username "tlh"
      yaoddmuse-directory (etc-path "yaoddmuse/"))


;; ;;; fuzzy
;; (add-path (site-path "fuzzy"))
;; (require 'fuzzy)


;;; undo-tree

(add-path (site-path "undo-tree"))
(require 'undo-tree)
(global-undo-tree-mode)


;;; zone

(require 'zone)
(zone-when-idle -1)


;;; malyon

(add-path (site-path "malyon"))

(require 'malyon)


;;; goto-last-change

(add-path (site-path "goto-last-change"))
(require 'goto-last-change)


;;; edit-server

(defun edit-server-backup ()
  (write-region
   (point-min) (point-max)
   (etc-path
    (format "edit-server-backups/edited-on-%s.txt"
            (replace-regexp-in-string " " "-" (current-time-string))))))

(when window-system
  (add-path (site-path "edit-server"))
  (require 'edit-server)
  (setq edit-server-new-frame nil)
  (add-hook 'edit-server-done-hook 'edit-server-backup)
  (or (process-status "edit-server")
      (edit-server-start))
  t)


;;; breadcrumb

(add-path (site-path "breadcrumb"))
(require 'breadcrumb)


;;; ascii-table

(add-path (site-path "ascii-table"))
(require 'ascii-table)


;;; provide

(provide 'tlh-mode)


;;; tlh-mode.el ends here
