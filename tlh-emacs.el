(byte-compile-disable-warning 'cl-functions)

(eval-when-compile
  (require 'cl))

(defvar home-dir
  (case system-type
    (darwin "/Users/luke/")
    ((linux gnu/linux) "/home/luke/")))

(defmacro defpathfn (name path)
  `(defun ,name (&optional sub)
     (expand-file-name (concat ,path (or sub "")))))

(defpathfn home-path    home-dir)
(defpathfn emacs-path   (home-path  "emacs/"))
(defpathfn elisp-path   (emacs-path "elisp/"))
(defpathfn init-path    (elisp-path "init/"))
(defpathfn private-path (elisp-path "private/"))
(defpathfn site-path    (emacs-path "site-lisp/"))
(defpathfn etc-path     (emacs-path "etc/"))

(defun add-path (path)
  (add-to-list 'load-path path))

(defun add-paths (&rest paths)
  (mapc 'add-path paths))

(add-paths (elisp-path "tlh-util/") (init-path) (private-path))

(mapc 'require '(tlh-util
                 tlh-init
                 tlh-mode
                 tlh-alias
                 tlh-keys
                 tlh-registers
                 tlh-startup))
