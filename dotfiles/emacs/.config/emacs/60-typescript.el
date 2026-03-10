(defun tennyson/typescript-hook ()
  (progn)
  (evil-surround-mode 1)
  (visual-line-mode 1)
  )


(add-hook 'typescript-mode-hook 'tennyson/typescript-hook 'append)
