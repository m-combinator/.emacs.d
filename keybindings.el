
;;Some key bindings to make things easier for me
(global-set-key (kbd "C-x p") 'list-packages)


(defun other-window-backward ()
  "Moves point to the previous window,
  or the nth previous window if n is not nil"
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-x C-p") 'other-window-backward)
