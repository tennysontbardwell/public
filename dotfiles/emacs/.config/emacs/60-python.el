(defun tennyson/py-mode-hook ()
  (setq evil-shift-width 4))

(setq python-indent-offset 4)
(add-hook 'python-mode-hook 'tennyson/py-mode-hook)
