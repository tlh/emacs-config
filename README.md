# emacs-config

This repo contains most of my emacs init files.  The entry point is
`tlh-emacs.el`.  My `~/.emacs` contains only this line:

    (load "~/emacs/elisp/init/tlh-emacs.el")

These files depend on [tlh-util.el][].  A lot of my other elisp code
has been broken out into separate packages, like: [recs-mode.el][],
[workgroups.el][], [tlh-color-themes][], [emms-info-id3v2.el][] and
[pickel.el][].

 [tlh-util.el]:         http://github.com/tlh/tlh-util.el
 [recs-mode.el]:        http://github.com/tlh/recs-mode.el
 [workgroups.el]:       http://github.com/tlh/workgroups.el
 [tlh-color-themes]:    http://github.com/tlh/tlh-color-themes
 [emms-info-id3v2.el]:  http://github.com/tlh/emms-info-id3v2.el
 [pickel.el]:           http://github.com/tlh/pickel.el
