;; https://kagi.com/assistant/503d0002-e0fe-4637-a239-a3bbc7d67655
(defun tennyson/setup-terminal-keys ()
  "Set up terminal-like keybindings in comint modes, overriding evil."
  (let ((key-bindings '(("C-a" . beginning-of-line)
                        ("C-e" . end-of-line)
                        ("C-p" . comint-previous-input)
                        ("C-n" . comint-next-input)
                        ("C-k" . kill-line)
                        ;; ("C-d" . delete-char)
                        ("C-c" . comint-interrupt-subjob)
                        ("C-l" . recenter-top-bottom)
                        ("C-r" . comint-history-isearch-backward))))
    (dolist (binding key-bindings)
      (let ((key (car binding))
            (command (cdr binding)))
        ;; Override in both insert and normal states
        (evil-local-set-key 'insert (kbd key) command)
        (evil-local-set-key 'normal (kbd key) command)))))

;; Apply to inferior-ess-r-mode
(add-hook 'inferior-ess-r-mode-hook 'tennyson/setup-terminal-keys)
