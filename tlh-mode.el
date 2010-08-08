;; comint

(setq comint-scroll-show-maximum-output nil
      comint-scroll-to-bottom-on-input  nil
      comint-prompt-read-only           t)

;; recentf

(recentf-mode 1)
(setq recentf-save-file (etc-path "recentf"))

;; saveplace

(require 'saveplace)
(setq save-place-file (etc-path "saved-places"))

;; whitespace-mode

(setq whitespace-line-column 100
      whitespace-style '(trailing
                         space-before-tab
                         indentation
                         space-after-tab))

;; text-mode

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; coding-hook

(defun coding-hook ()
  (setq save-place t)
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t)
  (whitespace-mode t)
  (add-watchwords))

;; emacs-lisp-mode

(setq eval-expression-print-level   20
      eval-expression-print-length  20)

(add-hooks 'emacs-lisp-mode-hook
           'turn-on-eldoc-mode
           'remove-elc-on-save
           'coding-hook
           'unicode-lambdas
           'cleanup-buffer-on-save)

;; lisp-mode

(add-hooks 'lisp-mode-hook
           'coding-hook
           'unicode-lambdas
           'cleanup-buffer-on-save)

;; windmove

(require 'windmove)
(setq windmove-wrap-around t)

;; browse-kill-ring

(add-path (site-path "browse-kill-ring/"))
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(setq browse-kill-ring-quit-action 'save-and-restore)

;; imenu

(require 'imenu)
(setq imenu-auto-rescan t)

;; ido

(when (> emacs-major-version 21)
  (ido-mode t)
  (ido-everywhere 1)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point nil
        ido-max-prospects 10
        ido-save-directory-list-file (etc-path "ido.last")))

(defun ido-imenu ()
  "Query with `ido-completing-read' a symbol in the buffer's
imenu index, then jump to that symbol's location."
  (interactive)
  (goto-char
   (let ((lst (nreverse
               (flatten-assoc-tree
                (imenu--make-index-alist)
                'imenu--subalist-p))))
     (access (ido-completing-read "Symbol: " (mapcar 'car lst)) lst))))

(defun ido-cache (pred &optional recalc)
  "Create a cache of symbols from `obarray' named after the
predicate PRED used to filter them."
  (let ((cache (intern (concat "ido-cache-" (symbol-name pred)))))
    (when (or recalc (not (boundp cache)))
      (set cache nil)
      (mapatoms (lambda (s)
                  (when (funcall pred s)
                    (push (symbol-name s) (symbol-value cache))))))
    (symbol-value cache)))

(defun ido-recalculate-all-caches ()
  "Recalculate the `ido-cache' of `functionp', `commandp' and
`boundp'."
  (interactive)
  (ido-cache 'commandp  t)
  (ido-cache 'functionp t)
  (ido-cache 'boundp    t)
  t)

(defun ido-execute-extended-command ()
  "ido replacement for `execute-extended-command'."
  (interactive)
  (call-interactively
   (intern (ido-completing-read "M-x " (ido-cache 'commandp)))))

(defun ido-describe-function (&optional at-point)
  "ido replacement for `describe-function'."
  (interactive "P")
  (describe-function
   (intern
    (ido-completing-read
     "Describe function: "
     (ido-cache 'functionp) nil nil
     (aand at-point (function-called-at-point) (format "%S" it))))))

(defun ido-describe-function-at-point ()
  (interactive)
  (ido-describe-function t))

(defun ido-find-function ()
  (interactive)
  (find-function (intern (ido-completing-read "Function: " (ido-cache 'functionp)))))

(defun ido-find-documentation-for-command ()
  "ido replacement for `find-documentation-for-command'."
  (interactive)
  (Info-goto-emacs-command-node
   (intern
    (ido-completing-read
     "Find documentation for command: " (ido-cache 'commandp)))))

(defun ido-describe-variable (&optional at-point)
  "ido replacement for `describe-variable'."
  (interactive "P")
  (describe-variable
   (intern
    (ido-completing-read
     "Describe variable: "
     (ido-cache 'boundp) nil nil
     (aand at-point (thing-at-point 'symbol) (format "%s" it))))))

(defun ido-describe-variable-at-point ()
  (interactive)
  (ido-describe-variable t))

(defun ido-sudo-find-file (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun ido-recentf-find-file ()
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file (find-file file))))

;; dired

(require 'dired)

(fill-keymap dired-mode-map "C-c w" 'wdired-change-to-wdired-mode)

(setq directory-free-space-program        "df"
      directory-free-space-args           "-h"
      dired-auto-revert-buffer            t
      wdired-allow-to-change-permissions  t)

;; eshell

(require 'eshell)

(defpathfn eshell-path (etc-path "eshell/"))

(setq eshell-directory-name              (eshell-path)
      eshell-aliases-file                (eshell-path "alias")
      eshell-history-file-name           (eshell-path "history")
      eshell-last-dir-ring-file-name     (eshell-path "lastdir")
      eshell-ls-use-in-dired             t
      eshell-save-history-on-exit        t
      eshell-scroll-show-maximum-output  nil
      eshell-scroll-to-bottom-on-output  nil
      eshell-cmpl-cycle-completions      nil)

;; workgroups

(add-path (elisp-path "workgroups/"))
(require 'workgroups)
(setq workgroups-configs-file (etc-path "workgroups-configs"))

;; recs

(add-path (elisp-path "recs/"))
(require 'recs)
(recs-mode t)
(setq recs-suggestion-interval   nil
      recs-ding-on-suggestion    nil
      recs-suggestion-window     t
      recs-suppress-suggestion   nil
      recs-window-select         t
      recs-log-suggestions       t
      recs-log-file              (etc-path "recs-log"))

(add-hook 'recs-hook 'yell-at-me)

;; uniquify

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator "|"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; acct

(require 'epa-file)
(add-path (elisp-path "acctdb/"))
(require 'acctdb)
;; (setq acctdb-file (expand-file-name "~/tcvol1/.accts.gpg"))
(setq acctdb-file (expand-file-name "~/tcvol1/.accts"))

;; tls

(require 'tls)
(setq tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof"))

;; erc

(require 'erc)
(require 'tlh-erc)

(defun erc-freenode-ssl-connect ()
  (interactive)
  (erc-tls :server "irc.freenode.net"
           :port 7070
           :nick "thunk"
           :full-name "thunk"
           :password (acctdb-get-pass "freenode")))

(defun erc-text-matched (match-type nick msg)
  (case match-type
    (current-nick
     (unless (string-match "^\\** Users on #" msg)
       (notify "ERC:" msg)))
    (pal
     (let ((nick (car (split-string nick "!"))))
       (when (member nick erc-pals)
         (message (format "%s: %s" nick msg)))))))

(add-hook 'erc-text-matched-hook 'erc-text-matched)

(defun switch-to-erc-buffer ()
  (interactive)
  (ido-buffer-internal ido-default-buffer-method nil nil nil "#"))

(defun next-erc-buffer (&optional prev)
  (interactive)
  (aif (sort-buffer-list (erc-buffer-list))
       (switch-to-buffer (funcall (if prev 'cprev 'cnext) (current-buffer) it))
       (message "There are no erc buffers.")))

(defun prev-erc-buffer ()
  (interactive)
  (next-erc-buffer t))

;; tramp

(require 'tramp)
(setq tramp-persistency-file-name (etc-path "tramp/tramp-persistence")
      tramp-auto-save-directory (etc-path "tramp/"))

;; gnus

(setq gnus-init-file (elisp-path "tlh-gnus.el"))
;; (setq gnus-startup-file (elisp-path "newsrc.el"))

;; midnight-mode

(require 'midnight)
(midnight-delay-set 'midnight-delay "7:00AM")

;; ansi-term

(setq ansi-term-color-vector [unspecified "black" "red3" "lime green" "yellow3"
                                          "DeepSkyBlue3" "magenta3" "cyan3" "white"])

;; paredit

(add-path (site-path "paredit"))
(autoload 'paredit-mode "paredit" nil t)

;; (add-hook 'lisp-mode-hook (lambda () (paredit-mode t)))
;; (add-hook 'clojure-mode-hook (lambda () (paredit-mode t)))

;; js2-mode

(add-path (site-path "js2-mode"))
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?$" . js2-mode))

;; clojure-mode

(add-path (site-path "clojure-mode"))
(autoload 'clojure-mode "clojure-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-hook 'clojure-mode-hook
          'coding-hook
          'unicode-lambdas
          'cleanup-buffer-on-save)

;; slime

(setq slime-lisp-implementations
      `((sbcl ("/usr/local/bin/sbcl" "--core" ,(etc-path "sbcl.core-for-slime")))
        (clozure ("/usr/bin/dx86cl64"))))

(add-path (site-path "slime"))
(require 'slime-autoloads)

;; other contribs: slime-references slime-scratch
;;                 slime-editing-commands slime-repl
;;                 inferior-slime-mode

(slime-setup '(slime-fancy slime-asdf slime-banner))
(when (featurep 'slime-autodoc) (unload-feature 'slime-autodoc))
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))

;; magit

(add-path (site-path "magit"))
(autoload 'magit-status "magit" nil t)

;; color-theme

(when window-system
  (add-paths (site-path "color-theme")
             (elisp-path "color-themes/"))
  (setq color-theme-load-all-themes nil)
  (require 'color-theme)
  (require 'color-theme-thunk1)
  (color-theme-initialize)
  (color-theme-thunk1))

;; jd-el

(add-path (site-path "jd-el"))
(autoload 'rainbow-mode "rainbow-mode" nil t)
(autoload 'google-maps "google-maps" nil t)

;; url vars

(defvar url-regexp "https?://")
(defvar google-search-string "http://www.google.com/search?q=")
(defvar google-lucky-search-string "http://www.google.com/search?btnI=I'm+Feeling+Lucky&q=")

;; w3m

(add-path (site-path "w3m"))

(require 'w3m-load)
;; (require 'mime-w3m)

(setq w3m-key-binding 'info
      w3m-home-page "about:"
      w3m-default-directory (etc-path "w3m/")
      w3m-profile-directory (etc-path "w3m/"))

(defun w3m-emacswiki-new-session (topic)
  (interactive "sTopic: ")
  (w3m-goto-url-new-session
   (concat google-lucky-search-string "site:emacswiki.org+" topic)))

;; browse-url

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

(defun browse-prev-url (&optional num)
  (interactive "P")
  (browse-url-find (lambda () (search-backward-regexp url-regexp)) num))

(defun browse-next-url (&optional num)
  (interactive "P")
  (browse-url-find (lambda () (search-forward-regexp url-regexp)) num))

(defun google-search (terms)
  (interactive (list (read-from-minibuffer "Query: ")))
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

;; google-define

(add-path (site-path "google-define"))
(require 'google-define)

;; yaoddmuse

(add-path (site-path "yaoddmuse"))
(require 'yaoddmuse)
(setq yaoddmuse-username "tlh"
      yaoddmuse-directory (etc-path "yaoddmuse/"))

;; emms

(add-path (site-path "emms/lisp"))
(require 'emms-setup)
(require 'emms-player-mplayer)
(emms-standard)
(add-to-list 'emms-player-list 'emms-player-mplayer)

(setq emms-source-file-default-directory (home-path "Music/iTunes/iTunes Media/Music/")
      emms-repeat-playlist t
      emms-playlist-default-major-mode 'emms-playlist-mode
      emms-cache-file (etc-path "emms/cache"))

(provide 'tlh-mode)
