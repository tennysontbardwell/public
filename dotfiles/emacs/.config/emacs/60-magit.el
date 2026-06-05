(defun spacemacs/git-permalink ()
  "Allow the user to get a permalink via git-link in a git-timemachine buffer."
  (interactive)
  (let ((git-link-use-commit t))
    (call-interactively 'git-link)))

(defun spacemacs/git-permalink-copy-url-only ()
  "Allow the user to get a permalink via git-link in a git-timemachine buffer."
  (interactive)
  (let (git-link-open-in-browser
        (git-link-use-commit t))
    (call-interactively 'git-link)))
