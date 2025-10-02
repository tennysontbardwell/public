;; GC ;;
(setq garbage-collection-messages t)
(setq gc-cons-threshold 402653184)

;; Undo ;;
(setq undo-tree-auto-save-history nil)
(undo-fu-session-global-mode -1)
(setq global-undo-tree-mode -1)
(setq evil-undo-system 'undo-redo)
(evil-set-undo-system 'undo-redo)
;; (add-to-list 'undo-tree-incompatible-major-modes #'org-mode)
(defun tennyson/discard-undo-history()
  (interactive)
  (setq buffer-undo-list nil) ; discard undo history
  (set-buffer-modified-p nil) ; mark the buffer as unmodified
  )


;; Tramp ;;
(setq tramp-terminal-type "dumb") ; Make Remote clients have a special variable set
(setq tramp-copy-size-limit nil)


;; Misc ;;
(setq dotspacemacs-search-tools '("rg" "ag" "ack" "grep"))
(setq calendar-week-start-day 1)
(global-activity-watch-mode 1)


;; pulsar
(pulsar-global-mode)
(setq
 pulsar-iterations 10
 pulsar-delay 0.015)


;; Backup Files ;;
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups


;; Ivy ;;
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
(ivy-posframe-mode 1)
(setq ivy-height 30)


;; GUI ;;
(setq spaceline-window-numbers-unicode nil)
(setq spaceline-workspace-numbers-unicode nil)
;; (setq max-mini-window-height 0.25)
(setq max-mini-window-height 1)


;; Web ;;
(setq-default web-mode-code-indent-offset 2)
;; (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))

;; https://github.com/orzechowskid/tsx-mode.el/issues/39
;; (add-to-list 'lsp--formatting-indent-alist '(tsx-mode . tsi-typescript-indent-offset))

(defun tennyson/web-mode-setup ()
  (setq standard-indent 2)
  (setq js-indent-level 2)
  (setq js-jsx-indent-level 2)
  (setq typescript-indent-level 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  )

(defun tennyson/ts-mode-setup ()
  (tennyson/web-mode-setup)
  (display-fill-column-indicator--turn-on)
  )

(add-hook 'js-mode-hook 'tennyson/web-mode-setup)
(add-hook 'typescript-mode-hook 'tennyson/ts-mode-setup)
(add-hook 'typescript-tsx-mode-hook 'tennyson/ts-mode-setup)

;; VC ;;
(setq vc-follow-symlinks t) ; Follow Symlinks w/o prompt


;; Vterm ;;
;; https://kagi.com/assistant/503d0002-e0fe-4637-a239-a3bbc7d67655
(defun tennyson/setup-terminal-keys ()
  "Set up terminal-like keybindings in comint modes, overriding evil."
  (let ((key-bindings
         '(
           ;; ("C-a" . vterm-send-C-a)
           ;; ("C-e" . vterm-send-C-e)
           ;; ("C-p" . comint-previous-input)
           ;; ("C-n" . comint-next-input)
           ;; ("C-k" . kill-line)
           ("C-y" . vterm-send-C-y)
           ;; ("C-d" . delete-char)
           ("C-c" . vterm-send-C-c)
           ;; ("C-l" . recenter-top-bottom)
           ;; ("C-r" . comint-history-isearch-backward)
           )))
    (dolist (binding key-bindings)
      (let ((key (car binding))
            (command (cdr binding)))
        ;; Override in both insert and normal states
        (evil-local-set-key 'insert (kbd key) command)
        (evil-local-set-key 'normal (kbd key) command)))))

(setq vterm-max-scrollback 10000
      vterm-timer-delay 0.01)
(add-hook 'vterm-mode-hook 'tennyson/setup-terminal-keys)


;; Nov ;;
(defun tennyson/nov-mode-hook ()
  (evil-motion-state))
(add-hook 'nov-mode-hook 'tennyson/nov-mode-hook)

;; EAF
(setq eaf-python-command "/Users/tennyson/.emacs.d/elpa/30.2/develop/eaf-20250829.10453/app/.venv/bin/python")

;; Unorganized ;;
