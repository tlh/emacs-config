;;;
;;;   ido
;;;


(ido-mode t)
(ido-everywhere 1)
(setq ido-enable-prefix             nil
      ido-enable-flex-matching      t
      ido-create-new-buffer        'always
      ido-use-filename-at-point     nil
      ido-max-prospects             50
      ido-use-faces                 t
      ido-max-window-height         nil
      ido-save-directory-list-file  (etc-path "ido.last")
      ido-default-file-method       'selected-window
      ido-default-buffer-method     'selected-window)

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
     (aand at-point (function-called-at-point)
           (symbol-name it))))))

(defun ido-yank ()
  "Select a kill to yank with `ido-completing-read'."
  (interactive)
  (insert-for-yank (ido-completing-read "Select kill: " kill-ring)))

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


;;; provide

(provide 'tlh-ido)


;;; tlh-ido.el ends here
