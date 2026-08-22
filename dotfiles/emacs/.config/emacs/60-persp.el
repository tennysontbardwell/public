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
      (let (to-kill)
        (dolist (buf (persp-buffers persp))
          (when (and (buffer-live-p buf)
                     (not (member (buffer-name buf) my/persp-protected-buffers))
                     (not
                      (cl-some
                       (lambda (name)
                         (let ((other (persp-get-by-name name)))
                           (and other
                                (not (eq other persp))
                                (memq buf (persp-buffers other)))))
                       (persp-names))))
            (push buf to-kill)))

        (when to-kill
          (run-at-time
           0 nil
           (lambda (buffers)
             (let ((fallback (get-buffer-create "*scratch*")))
               (dolist (buf buffers)
                 (when (buffer-live-p buf)
                   (dolist (win (get-buffer-window-list buf nil t))
                     (purpose-set-window-purpose-dedicated-p win nil)
                     (set-window-dedicated-p win nil)
                     (set-window-buffer win fallback))
                   (kill-buffer buf)))))
           to-kill)))))

  (add-hook 'persp-before-kill-functions #'my/kill-persp-orphan-buffers))

(defun my/maximize-in-new-tab ()
  "Open a new tab containing only the current buffer, maximized."
  (interactive)
  (let ((buf (current-buffer)))
    (spacemacs/clone-workspace)
    (delete-other-windows)
    (switch-to-buffer buf)))
