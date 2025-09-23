(defun tennyson/cmd-in-term (&optional cmd dir terminal)
  "Open directory in terminal with optional command.
CMD: Command to run (if nil, opens interactive shell)
DIR: Directory to open (if nil, defaults to current buffer's directory or default-directory)
TERMINAL: Terminal application to use ('ghostty or 'iterm, defaults to 'ghostty)"
  (interactive)
  (let* ((target-dir (or dir
                         (if (buffer-file-name)
                             (file-name-directory (buffer-file-name))
                           (expand-file-name default-directory))))
         (terminal-app (or terminal 'ghostty))
         (app-name (if (eq terminal-app 'iterm) "iTerm" "Ghostty"))
         (tmux-cmd (if cmd
                       (format "tmux new-session -A -s main; tmux new-window -c %s -t main %s"
                               (shell-quote-argument target-dir)
                               (shell-quote-argument cmd))
                     (format "tmux new-session -A -s main; tmux new-window -c %s -t main"
                             (shell-quote-argument target-dir))))
         (full-cmd (format "%s; osascript -e 'tell application \"%s\" to activate'"
                           tmux-cmd app-name)))
    (start-process-shell-command
     "open-terminal"
     nil
     full-cmd)))

;; ==== Copying to System Clipboard

(defun tennyson-copy-to-clipboard ()
  (interactive)
  (if (eq system-type 'gnu/linux)
      (progn
        (call-process-region (point) (mark) "xsel" nil nil nil "-b")
        (setq deactivate-mark t))
    (progn
      (call-process-region (point) (mark) "pbcopy")
      (setq deactivate-mark t))))

(defun tennyson-paste-to-clipboard ()
  (interactive)
  (if (eq system-type 'gnu/linux)
      (call-process-region (point) (if mark-active (mark) (point)) "xsel" t t nil "-ob")
    (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t)))

(defun tennyson-cut-to-clipboard ()
  (interactive)
  (tennyson-copy-to-clipboard)
  (delete-region (region-beginning) (region-end)))
