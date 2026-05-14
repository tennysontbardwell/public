(setq-default web-mode-code-indent-offset 2)
;; (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))

;; https://github.com/orzechowskid/tsx-mode.el/issues/39
;; (add-to-list 'lsp--formatting-indent-alist '(tsx-mode . tsi-typescript-indent-offset))

(defun tennyson/web-mode-setup ()
  (setq standard-indent 2)
  (setq js-indent-level 2)
  (setq js-jsx-indent-level 2)
  (setq typescript-indent-level 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)

  (evil-surround-mode1)
  )

(defun tennyson/ts-mode-setup ()
  (tennyson/web-mode-setup)
  (display-fill-column-indicator-mode t)
  )

(add-hook 'js-mode-hook 'tennyson/web-mode-setup)
(add-hook 'typescript-mode-hook 'tennyson/ts-mode-setup)
(add-hook 'typescript-tsx-mode-hook 'tennyson/ts-mode-setup)
