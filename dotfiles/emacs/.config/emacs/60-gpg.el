(defun fov/disable-backups-for-gpg ()
  "Disable backups and autosaving for files ending in \".gpg\"."
  (when (and (buffer-file-name)
             (s-ends-with-p ".gpg" (buffer-file-name) t))
    (setq-local backup-inhibited t)
    (setq-local undo-tree-auto-save-history nil)
    (auto-save-mode -1)))
(add-hook 'find-file-hook #'fov/disable-backups-for-gpg)
