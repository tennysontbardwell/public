;; ;; ==== Scrolling fix for help buffer
;; (setq evil-want-C-u-scroll t)
;;
;; (add-hook 'help-mode-hook
;;           (lambda() (local-set-key (kbd "C-u") 'evil-scroll-up)))
;;
;; (global-set-key (kbd "C-u") 'evil-scroll-up)
;;
;; (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
;; (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
;; (define-key evil-insert-state-map (kbd "C-u")
;;             (lambda ()
;;               (interactive)
;;               (evil-delete (point-at-bol) (point))))
;;
;; (define-key evil-normal-state-map (kbd "RET") 'spacemacs/evil-insert-line-below)



;; ;; hyperbol
;; disables `/r/` to `/mnt/r` translation
;; (defun hpath:mswindows-to-posix (path) path)



;; (with-eval-after-load "persp-mode"
;;   (defvar after-switch-to-buffer-functions nil)
;;   (defvar after-display-buffer-functions nil)
;;
;;   (if (fboundp 'advice-add)
;;       ;;Modern way
;;       (progn
;;         (defun after-switch-to-buffer-adv (&rest r)
;;           (apply #'run-hook-with-args 'after-switch-to-buffer-functions r))
;;         (defun after-display-buffer-adv (&rest r)
;;           (apply #'run-hook-with-args 'after-display-buffer-functions r))
;;         (advice-add #'switch-to-buffer :after #'after-switch-to-buffer-adv)
;;         (advice-add #'display-buffer   :after #'after-display-buffer-adv))
;;
;;     ;;Old way
;;     (defadvice switch-to-buffer (after after-switch-to-buffer-adv)
;;       (run-hook-with-args 'after-switch-to-buffer-functions (ad-get-arg 0)))
;;     (defadvice display-buffer (after after-display-buffer-adv)
;;       (run-hook-with-args 'after-display-buffer-functions (ad-get-arg 0)))
;;     (ad-enable-advice #'switch-to-buffer 'after 'after-switch-to-buffer-adv)
;;     (ad-enable-advice #'display-buffer 'after 'after-display-buffer-adv)
;;     (ad-activate #'switch-to-buffer)
;;     (ad-activate #'display-buffer)))
;;
;; (add-hook 'after-switch-to-buffer-functions
;;           #'(lambda (bn) (when (and persp-mode
;;                                     (not persp-temporarily-display-buffer))
;;                            (persp-add-buffer bn))))



;; (defhydra tbardwell/hydra-pdf (:color pink)
;;   "pdf program to open"
;;   ("d" open-default-app "default")
;;   ("s" open-sioyek "sioyek")
;;   ("p" open-mac-preview "macos preview")
;;   ("q" nil "quit")
;;   )



;; (defun tbardwell/openwith-file-handler (operation &rest args)
;;   "Open file with external program, if an association is configured."
;;   (let ((continue t))
;;     (when (and tbardwell/openwith-mode (not (buffer-modified-p)) (zerop (buffer-size)))
;;       (let ((file (car args)))
;;         (when (save-match-data (string-match "\\.pdf\\'" file))
;;           (message "pdf blocked")
;;           ;; (kill-buffer nil)
;;           (spacemacs//open-in-external-app file)
;;           ;; (setq tbardwell/open-with/temp-file-name file)
;;           ;; (tbardwell/hydra-pdf/body)
;;           (setq continue nil)
;;           (when (featurep 'recentf)
;;             (recentf-add-file file))
;;           )))
;;     (when continue
;;       (message "continue")
;;       ;; when no association was found, relay the operation to other handlers
;;       (let ((inhibit-file-name-handlers
;;              (cons 'tbardwell/openwith-file-handler
;;                    (and (eq inhibit-file-name-operation operation)
;;                         inhibit-file-name-handlers)))
;;             (inhibit-file-name-operation operation))
;;         (apply operation args)))))



;; (define-minor-mode tbardwell/openwith-mode
;;   "Automatically open files with external programs."
;;   :lighter ""
;;   :global t
;;   (if tbardwell/openwith-mode
;;       (progn
;;         ;; register `openwith-file-handler' for all files
;;         (put 'tbardwell/openwith-file-handler 'safe-magic t)
;;         (put 'tbardwell/openwith-file-handler 'operations '(insert-file-contents))
;;         (add-to-list 'file-name-handler-alist '("" . tbardwell/openwith-file-handler)))
;;     (setq file-name-handler-alist
;;           (delete '("" . tbardwell/openwith-file-handler) file-name-handler-alist))))



;; (setq pdf-view-restore-filename "~/.emacs.d/.pdf-view-restore")
;; (defun open-sioyek ()
;;   shell-command (format "nohup sioyek \"%s\" > /dev/null &" tbardwell/open-with/temp-file-name))
;; (defun open-mac-preview ()
;;   shell-command (format "open -a /System/Applications/Preview.app \"%s\"" tbardwell/open-with/temp-file-name))



;; (defun set-selective-display-dlw (&optional level)
;;   "Fold text indented more than the cursor.
;;    If level is set, set the indent level to level.
;;    0 displays the entire buffer."
;;   (interactive "P")
;;   (let* ((base-column (current-column))
;;          (from-column (if (= base-column 0) 0 (1+ base-column))))
;;     (set-selective-display (or level from-column))))



;; (defun open-default-app ()
;;   (interactive)
;;   (spacemacs//open-in-external-app tbardwell/open-with/temp-file-name))



;; Open using external apps, not subprocesses
;; (setq browse-url-browser-function 'browse-url-generic browse-url-generic-program "xdg-open")




;; (defun tennnyson/org-advanced ()
;;   (interactive)
;;  (defun org-cycle-internal-local ()
;;    "Do the local cycling action."
;;    (let ((goal-column 0) eoh eol eos has-children children-skipped struct)
;;      ;; First, determine end of headline (EOH), end of subtree or item
;;      ;; (EOS), and if item or heading has children (HAS-CHILDREN).
;;      (save-excursion
;;        (if (org-at-item-p)
;;      (progn
;;        (beginning-of-line)
;;        (setq struct (org-list-struct))

;;        (setq eoh (point-at-eol))
;;        (setq eos (org-list-get-item-end-before-blank (point) struct))
;;        (setq has-children (org-list-has-child-p (point) struct)))
;;    (org-back-to-heading)
;;    (setq eoh (save-excursion (outline-end-of-heading) (point)))
;;    (setq eos (save-excursion
;;          (org-end-of-subtree t t)
;;          (unless (eobp) (forward-char -1))
;;          (point)))
;;    (setq has-children
;;          (or
;;          (save-excursion
;;      (let ((level (funcall outline-level)))
;;        (outline-next-heading)
;;        (and (org-at-heading-p t)
;;        (> (funcall outline-level) level))))
;;          (and (eq org-cycle-include-plain-lists 'integrate)
;;          (save-excursion
;;            (org-list-search-forward (org-item-beginning-re) eos t))))))
;;        ;; Determine end invisible part of buffer (EOL)
;;        (beginning-of-line 2)
;;        (while (and (not (eobp))		;this is like `next-line'
;;        (get-char-property (1- (point)) 'invisible))
;;    (goto-char (next-single-char-property-change (point) 'invisible))
;;    (and (eolp) (beginning-of-line 2)))
;;        (setq eol (point)))
;;      ;; Find out what to do next and set `this-command'
;;      (cond
;;      ((= eos eoh)
;;        ;; Nothing is hidden behind this heading
;;        (unless (org-before-first-heading-p)
;;    (run-hook-with-args 'org-pre-cycle-hook 'empty))
;;        (org-unlogged-message "EMPTY ENTRY")
;;        (setq org-cycle-subtree-status nil)
;;        (save-excursion
;;    (goto-char eos)
;;    (outline-next-heading)
;;    (when (org-invisible-p) (org-flag-heading nil))))
;;      ((and (or (>= eol eos)
;;          (not (string-match "\\S-" (buffer-substring eol eos))))
;;      (or has-children
;;          (not (setq children-skipped
;;          org-cycle-skip-children-state-if-no-children))))
;;        ;; Entire subtree is hidden in one line: children view
;;        (unless (org-before-first-heading-p)
;;    (run-hook-with-args 'org-pre-cycle-hook 'children))
;;        (if (org-at-item-p)
;;      (org-list-set-item-visibility (point-at-bol) struct 'children)
;;    (org-show-entry)
;;    (org-with-limited-levels (org-show-children))
;;    (org-show-set-visibility 'tree)
;;    ;; Fold every list in subtree to top-level items.
;;    (when (eq org-cycle-include-plain-lists 'integrate)
;;      (save-excursion
;;        (org-back-to-heading)
;;        (while (org-list-search-forward (org-item-beginning-re) eos t)
;;          (beginning-of-line 1)
;;          (let* ((struct (org-list-struct))
;;          (prevs (org-list-prevs-alist struct))
;;          (end (org-list-get-bottom-point struct)))
;;      (dolist (e (org-list-get-all-items (point) struct prevs))
;;        (org-list-set-item-visibility e struct 'folded))
;;      (goto-char (if (< end eos) end eos)))))))
;;        (org-unlogged-message "CHILDREN")
;;        (save-excursion
;;    (goto-char eos)
;;    (outline-next-heading)
;;    (when (org-invisible-p) (org-flag-heading nil)))
;;        (setq org-cycle-subtree-status 'children)
;;        (unless (org-before-first-heading-p)
;;    (run-hook-with-args 'org-cycle-hook 'children)))
;;      ((or children-skipped
;;      (and (eq last-command this-command)
;;          (eq org-cycle-subtree-status 'children)))
;;        ;; We just showed the children, or no children are there,
;;        ;; now show everything.
;;        (unless (org-before-first-heading-p)
;;    (run-hook-with-args 'org-pre-cycle-hook 'subtree))
;;        (org-flag-region eoh eos nil 'outline)
;;        (org-unlogged-message
;;        (if children-skipped "SUBTREE (NO CHILDREN)" "SUBTREE"))
;;        (setq org-cycle-subtree-status 'subtree)
;;        (unless (org-before-first-heading-p)
;;    (run-hook-with-args 'org-cycle-hook 'subtree)))
;;      ((eq org-cycle-subtree-status 'subtree)
;;         (org-show-subtree)
;;           (org-unlogged-message "ALL")
;;             (setq org-cycle-subtree-status 'all))
;;      (t
;;        ;; Default action: hide the subtree.
;;        (run-hook-with-args 'org-pre-cycle-hook 'folded)
;;        (org-flag-region eoh eos t 'outline)
;;        (org-unlogged-message "FOLDED")
;;        (setq org-cycle-subtree-status 'folded)
;;        (unless (org-before-first-heading-p)
;;    (run-hook-with-args 'org-cycle-hook 'folded))))))

;; (defun org-cycle-hide-drawers (state)
;;   "Re-hide all drawers after a visibility state change."
;;   (when (and (derived-mode-p 'org-mode)
;;             (not (memq state '(overview folded contents))))
;;     (save-excursion
;;       (let* ((globalp (memq state '(contents all)))
;;             (beg (if globalp
;;                     (point-min)
;;                     (point)))
;;             (end (if globalp
;;                     (point-max)
;;                     (if (eq state 'children)
;;                       (save-excursion
;;                         (outline-next-heading)
;;                         (point))
;;                       (org-end-of-subtree t)))))
;;         (goto-char beg)
;;         (while (re-search-forward org-drawer-regexp end t)
;;           (save-excursion
;;             (beginning-of-line 1)
;;             (when (looking-at org-drawer-regexp)
;;               (let* ((start (1- (match-beginning 0)))
;;                     (limit
;;                       (save-excursion
;;                         (outline-next-heading)
;;                           (point)))
;;                     (msg (format
;;                             (concat
;;                               "org-cycle-hide-drawers:  "
;;                               "`:END:`"
;;                               " line missing at position %s")
;;                             (1+ start))))
;;                 (if (re-search-forward "^[ \t]*:END:" limit t)
;;                   (outline-flag-region start (point-at-eol) t)
;;                   (user-error msg))))))))))
;; )



;; ;; ; Capture Templates
;; ;; (defun tennyson/finalize-capture-hook ()
;; ;;       (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
;; ;;              (final-text (replace-in-string "mail/u/0/#inbox/" "mail/u/0/#all/" text)))
;; ;;         (save-excursion
;; ;;           (save-restriction
;; ;;             (delete-region (point-min) (point-max))
;; ;;             (insert final-text)))))

;; ;; Capture Templates
;; (defun tennyson/finalize-capture-hook (&rest _args) ignore)

;; (add-hook 'org-capture-prepare-finalize-hook 'tennyson/finalize-capture-hook)

;; (setq org-capture-templates
;;       '(("w"
;;          "Default template"
;;          entry
;;          (file+headline "~/repos/tennysontennyson/personal/todo.org" "1 (today)")
;;          "* %^{Title}\n\n  Source: %u, %c\n\n  %i"
;;          :empty-lines 1)
;;         ("t"
;;          "Todo"
;;          entry
;;          (file+headline "~/repos/tennysontennyson/personal/todo.org" "0 (inbox)")
;;          "* TODO %?[[%:link][%:description]]\n%i"
;;          :prepend nil
;;          :immediate-finish nil
;;          :prepare-finalize 'tennyson/finalize-capture-hook)
;;         ))



;; https://www.reddit.com/r/emacs/comments/9htd0r/how_to_completely_hide_the_properties_drawer_in/
;; (defun org-toggle-properties ()
;;   ;; toggle visibility of properties in current header if it exists
;;   (interactive)
;;   (save-excursion
;;     (when (not (org-at-heading-p))
;;       (org-previous-visible-heading 1))
;;     (when (org-header-property-p)
;;       (let* ((a (re-search-forward "\n\\:" nil t)))
;;         (if (outline-invisible-p (point))
;;             (outline-show-entry)
;;           (org-cycle-hide-drawers 'all))))))
