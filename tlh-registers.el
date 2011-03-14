;; registers

(fill-registers 'file
                ?c (init-path  "tlh-osx.el")
                ?e (init-path  "tlh-emacs.el")
                ?g (init-path  "tlh-gnus.el")
                ?i (init-path  "tlh-init.el")
                ?j (etc-path   "junk")
                ?k (init-path  "tlh-keys.el")
                ?m (init-path  "tlh-mode.el")
                ?n (home-path  "tcvol1/.notes.org")
                ?r (init-path  "tlh-registers.el")
                ?s (elisp-path "recs-mode/recs-mode.el")
                ?t (elisp-path "color-themes/color-theme-thunk1.el")
                ?u (elisp-path "tlh-util/tlh-util.el")
                ?v (elisp-path "kvdb/kvdb.el")
                ?w (elisp-path "workgroups/workgroups.el")
                ?y (init-path  "tlh-system.el"))

(provide 'tlh-registers)
