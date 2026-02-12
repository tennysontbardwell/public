(defun spacemacs//ivy-persp-switch-project-action (project)
  (spacemacs||switch-project-persp project
    (if current-prefix-arg
        (dired project)
      (let ((default-directory (file-name-as-directory (expand-file-name project))))
        (counsel-projectile-find-file)))))
