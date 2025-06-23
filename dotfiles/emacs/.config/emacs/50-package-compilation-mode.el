(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;; this runs with the *compilation* buffer current.  see the documentation
;;; for `compilation-filter-hook'
(defun my/forget-compilation-errors ()
  (let ((inserted-string (buffer-substring-no-properties
                          compilation-filter-start (point))))
    (when (string-match-p "Changes detected. Rebuidling" inserted-string)
      (compilation-forget-errors))))

(add-hook 'compilation-filter-hook #'my/forget-compilation-errors)

(defadvice compilation-forget-errors (after reset-num-errors-found activate)
  (progn
    (setq-local compilation-num-errors-found 0)
    (setq-local compilation-num-warnings-found 0)
    (setq-local compilation-num-infos-found 0)))
