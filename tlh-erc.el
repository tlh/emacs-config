;;;
;;;   tlh-erc
;;;

(require 'erc)


;;; erc-highlight-nicknames

(add-path (site-path "erc-highlight-nicknames"))
(require 'erc-highlight-nicknames)
(add-to-list 'erc-modules 'highlight-nicknames)
(erc-update-modules)


;;; connection command

(defun erc-freenode-connect (&optional ssl)
  (interactive "P")
  (funcall (if ssl 'erc-tls 'erc)
           :server     "irc.freenode.net"
           :port       (if ssl 7070 6667)
           :nick       "thunk"
           :full-name  "thunk"
           :password   (kvdb-get-val "Accounts" "9" "password")))


;;; text matched hook

(defun erc-text-matched (match-type nick msg)
  (case match-type
    (current-nick
     (unless (string-match "^\\** Users on #" msg)
       (notify "ERC:" msg)))
    (keyword (notify "ERC:" msg))
    (pal
     (let ((nick (car (split-string nick "!"))))
       (when (member nick erc-pals)
         (message (format "%s: %s" nick msg)))))))

(add-hook 'erc-text-matched-hook 'erc-text-matched)

(push "workgroups" erc-keywords)


;;; erc buffer switching

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


;;; quit reason

(setq erc-quit-reason-various-alist
      '(("brb" "brb")))

(defun thunk-quit-reason (&optional n)
  (nth (or n 0)
       '("thunk"
         "I'm using zxvg 1.11 with GNU Emacs \
47b.1.70.3 (x512_128-statex-occuline37.2.0, \
para-tty) of 2020.1573.28374.")))


;;; set vars

(setq erc-modules '(autojoin button completion fill irccontrols
                             list log match menu move-to-prompt
                             netsplit networks noncommands
                             readonly ring services stamp track)
      erc-autojoin-channels-alist           '(("freenode" "#emacs" "#startups" "#clojure" "##cinema"))
      erc-away-nickname                      "thunk|away"
      erc-log-channels-directory             (etc-path "erc-logs/")
      erc-log-insert-log-on-open             nil
      erc-nick                               "thunk"
      erc-nick-uniquifier                    "`"
      erc-prompt-for-nickserv-password       nil
      erc-prompt-for-password                nil
      erc-public-away-p                      nil
      erc-quit-reason                       'thunk-quit-reason
      erc-part-reason                       'thunk-quit-reason
      erc-save-buffer-on-part                t
      erc-server-auto-reconnect              t
      erc-system-name                        "bine"
      erc-user-full-name                     "thunk"
      erc-hide-list                          nil
      erc-pals                               nil)


;;; provide

(provide 'tlh-erc)


;;; tlh-erc.el ends here
