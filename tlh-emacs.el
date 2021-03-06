;;; path fns and requires

(when (> emacs-major-version 22)
  (with-no-warnings
    (byte-compile-disable-warning 'cl-functions)))

(add-to-list 'load-path "~/emacs/elisp/tlh-util/")

(require 'tlh-util)

(defpathfn home-path     (expand-file-name "~/"))
(defpathfn emacs-path    (home-path  "emacs/"))
(defpathfn elisp-path    (emacs-path "elisp/"))
(defpathfn site-path     (emacs-path "site-lisp/"))
(defpathfn etc-path      (emacs-path "etc/"))
(defpathfn init-path     (elisp-path "init/"))
(defpathfn private-path  (elisp-path "private/"))

(add-paths (init-path) (private-path))

(mapc #'require '(tlh-util
                  tlh-init
                  tlh-mode
                  tlh-system
                  tlh-alias
                  tlh-keys
                  tlh-registers
                  tlh-startup))

;;; end tlh-emacs.el
