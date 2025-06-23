(defun tennyson/cmd-in-iterm (cmd)
  "Open the current directory in iTerm by creating a new tmux window in session 'main'."
  (interactive)
  (let ((dir (if (buffer-file-name)
                 (file-name-directory (buffer-file-name))
               default-directory)))
    (start-process-shell-command
     "open-iterm"
     nil
     (format "tmux new-session -A -s main; tmux new-window -c %s -t main %s; osascript -e 'tell application \"iTerm\" to activate'"
             (shell-quote-argument dir)
             (shell-quote-argument cmd)
             ))))


(defun tennyson/open-dir-in-iterm ()
  "Open the current directory in iTerm by creating a new tmux window in session 'main'."
  (interactive)
  (let ((dir (if (buffer-file-name)
                 (file-name-directory (buffer-file-name))
               (expand-file-name default-directory))))
    (start-process-shell-command
     "open-iterm"
     nil
     (format "tmux new-session -A -s main; tmux new-window -c %s -t main; osascript -e 'tell application \"iTerm\" to activate'"
             (shell-quote-argument dir)))))


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
