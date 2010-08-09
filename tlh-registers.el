;; registers

(fill-registers 'file
                ?a (elisp-path "acctdb/acctdb.el")
                ?c (init-path  "tlh-mac.el")
                ?e (init-path  "tlh-emacs.el")
                ?g (init-path  "tlh-gnus.el")
                ?i (init-path  "tlh-init.el")
                ?j (etc-path   "junk")
                ?k (init-path  "tlh-keys.el")
                ?m (init-path  "tlh-mode.el")
                ?n (home-path  "tcvol1/.notes")
                ?r (init-path  "tlh-registers.el")
                ?s (elisp-path "recs-mode/recs-mode.el")
                ?t (elisp-path "color-themes/color-theme-thunk1.el")
                ?u (elisp-path "tlh-util/tlh-util.el")
                ?w (elisp-path "workgroups/workgroups.el"))

(provide 'tlh-registers)
