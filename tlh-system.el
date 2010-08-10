;;; system specific

;; window-system

(when window-system
  (menu-bar-mode      -1)
  (tool-bar-mode      -1)
  (scroll-bar-mode    -1)
  (tooltip-mode       -1)
  (blink-cursor-mode  -1)
  (mouse-wheel-mode    t))

;; os

(case system-type
  ((gnu/linux linux)
   nil)
  (darwin
   (require 'tlh-osx)))

;; provide

(provide 'tlh-system)

;;; tlh-system.el ends here
