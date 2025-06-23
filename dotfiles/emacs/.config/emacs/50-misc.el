;; GC ;;
(setq garbage-collection-messages t)
(setq gc-cons-threshold 20000000)

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
(setq dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep"))
(setq calendar-week-start-day 1)
(global-activity-watch-mode 1)
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


;; GUI ;;
(setq spaceline-window-numbers-unicode nil)
(setq spaceline-workspace-numbers-unicode nil)


;; Web ;;
(setq-default web-mode-code-indent-offset 2)


;; VC ;;
(setq vc-follow-symlinks t) ; Follow Symlinks w/o prompt


;; Vterm ;;
(setq vterm-max-scrollback 10000
      vterm-timer-delay 0.01)


;; Nov ;;
(defun tennyson/nov-mode-hook ()
  (evil-motion-state))
(add-hook 'nov-mode-hook 'tennyson/nov-mode-hook)

;; Unorganized ;;
