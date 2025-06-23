(setq dired-listing-switches "-alh")

(use-package dired-subtree
  :bind (:map dired-mode-map
              ;; ("i" . dired-subtree-insert)
              ;; (";" . dired-subtree-remove)
              ("<tab>" . dired-subtree-toggle)
              ;; ("<backtab>" . dired-subtree-cycle)
              ))
