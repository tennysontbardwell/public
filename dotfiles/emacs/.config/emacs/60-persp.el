(defun spacemacs//ivy-persp-switch-project-action (project)
  (spacemacs||switch-project-persp project
    (if current-prefix-arg
        (dired project)
      (let ((default-directory (file-name-as-directory (expand-file-name project))))
        (counsel-projectile-find-file)))))

;; (setq persp-add-buffer-on-after-change-major-mode nil)

(with-eval-after-load 'persp-mode
  (defvar my/persp-protected-buffers
    '("*spacemacs*" "*scratch*" "*Messages*")
    "Buffer names never killed on persp close.")

  (defun my/kill-persp-orphan-buffers (persp)
    "Kill buffers unique to PERSP, leaving shared and protected ones alone."
    (when persp
      (dolist (buf (persp-buffers persp))
        (if (and (buffer-live-p buf)
                 (not (member (buffer-name buf) my/persp-protected-buffers)))
            (let ((shared
                   (cl-some
                    (lambda (name)
                      (let ((other (persp-get-by-name name)))
                        (and other
                             (not (eq other persp))
                             (memq buf (persp-buffers other)))))
                    (persp-names))))
              (unless shared
                (message "Buffer to kill: %s" (buffer-name buf))
                ;; (kill-buffer buf)
                ))
          (message "Buffer to leave: %s" (buffer-name buf))
          )
        )))

  (add-hook 'persp-before-kill-functions #'my/kill-persp-orphan-buffers))

(defun my/maximize-in-new-tab ()
  "Open a new tab containing only the current buffer, maximized."
  (interactive)
  (let ((buf (current-buffer)))
    (spacemacs/clone-workspace)
    (delete-other-windows)
    (switch-to-buffer buf)))
