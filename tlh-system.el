;;; system specific


;;; window-system

(when window-system
  (menu-bar-mode      -1)
  (tool-bar-mode      -1)
  (scroll-bar-mode    -1)
  (tooltip-mode       -1)
  (blink-cursor-mode  -1)
  (mouse-wheel-mode    t))


;;; system-type

(case system-type
  ((gnu/linux linux)
   (require 'tlh-linux))
  (darwin
   (require 'tlh-osx)))


;;; system-name

(case system-name
  ("bine.local" nil))


;;; user

(case user-login-name
  ("luke" nil))


;;; provide

(provide 'tlh-system)


;;; tlh-system.el ends here
