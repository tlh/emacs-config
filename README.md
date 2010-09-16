# emacs-config

This repo contains most of my emacs init files.  The entry point is
`tlh-emacs.el`.  My `~/.emacs` file contains only these lines:

    (add-to-list 'load-path "~/emacs/elisp/tlh-util/")
    (load "~/emacs/elisp/init/tlh-emacs.el")

The first line adds [tlh-util.el][] to `load-path`, which the rest of
these files depend on.  A lot of the other code has been broken out
into separate packages, like: [recs-mode.el][], [workgroups.el][],
[tlh-color-themes][], [emms-info-id3v2.el][].

 [tlh-util.el]:         http://github.com/tlh/tlh-util.el
 [recs-mode.el]:        http://github.com/tlh/recs-mode.el
 [workgroups.el]:       http://github.com/tlh/workgroups.el
 [tlh-color-themes]:    http://github.com/tlh/tlh-color-themes
 [emms-info-id3v2.el]:  http://github.com/tlh/emms-info-id3v2.el
