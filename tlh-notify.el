;;; notification

(defvar notify-function nil
  "A function that takes two arguments, a string TITLE and a
string MESSAGE, and creates a notification.")

(defun notify (title message)
  (if notify-function
      (funcall notify-function title message)
    (message "`notify-function' has not been set.")))

(provide 'tlh-notify)

;;; tlh-notify.el ends here
