;; (defun latex-mode ()
;;   (interactive ())
;;   (setq cdlatex-math-symbol-alist '())

;;   (setq
;;    cdlatex-command-alist
;;    '(
;;      ("m" "Insert multiline equation" "\\[\n  ?\n\\]" cdlatex-position-cursor nil t nil)
;;      ("tt" "Insert text" "\\text{?}" cdlatex-position-cursor nil nil t)
;;      ))

;;   (add-to-list
;;    'cdlatex-env-alist
;;    '(
;;      ("eq" nil "\[\]")
;;      ))
;;   )

;; (add-to-list 'auto-mode-alist '("\\.tex$" . latex-mode))
