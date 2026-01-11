;; Bugfix ;;

;; https://github.com/syl20bnr/spacemacs/issues/13465
(setq org-src-tab-acts-natively nil)

(defun tennyson/fix-org-hide-on-theme-change ()
  "Fix org-hide face after theme change."
  (set-face-attribute 'org-hide nil
                      :foreground (face-attribute 'default :background)
                      :background (face-attribute 'default :background)))
(add-hook 'spacemacs-post-theme-change-hook #'tennyson/fix-org-hide-on-theme-change)

(defun tennyson/org-pandoc-export-to-clipboard ()
  "Export the current region as markdown using org-pandoc and copy to clipboard."
  (interactive)
  (let ((original-buffer (current-buffer))
        (original-buffers (buffer-list))
        (org-pandoc-options-for-commonmark
         '(
           (standalone . nil)
           (wrap . none)
           )))
    (save-excursion
      (save-restriction
        (org-pandoc-export-as-commonmark)
        (let ((new-buffers (cl-set-difference (buffer-list) original-buffers)))
          (when new-buffers
            (let ((export-buffer (car new-buffers)))
              (let ((proc (get-buffer-process export-buffer)))
                (if proc
                    ;; Set a process sentinel to handle completion
                    (set-process-sentinel
                     proc
                     (lambda (process event)
                       (message "Waiting for proc to finish")
                       (when (string= event "finished\n")
                         (let ((buf (process-buffer process)))
                           (when (buffer-live-p buf)
                             (with-current-buffer buf
                               (kill-new (buffer-string))
                               (message "Exported to clipboard as markdown"))
                             (kill-buffer buf)))
                         ;; Clean up temp files (from org-pandoc-sentinel logic)
                         (dolist (file (process-get process 'files))
                           (when (and file (file-exists-p file))
                             (delete-file file)))
                         )))
                  ;; No process, handle immediately
                  (progn
                    (with-current-buffer export-buffer
                      (kill-new (buffer-string))
                      (message "Exported to clipboard as markdown"))
                    (kill-buffer export-buffer)))))))))))

;; Anesthetics ;;
(setq org-startup-indented t)
(setq org-src-window-setup 'other-window)


;; Performance ;;
(setq org-element-use-cache nil)


;; Babel ;;
(setq org-babel-python-command "python3")
(setq org-confirm-babel-evaluate nil)
(setq org-plantuml-exec-mode 'plantuml)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ledger . t)
   (julia . t)
   (plantuml . t)
   (shell . t)
   (dot . t)
   (R . t)
   (python . t)))

;; Export ;;
(setq org-export-with-smart-quotes t)


;; Behavior ;;
(setq org-id-link-to-org-use-id t)
(setq org-enable-priority-commands t
      org-highest-priority ?A
      org-default-priority ?J
      org-lowest-priority ?J)


;; Keys ;;
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "TAB") nil)
  (high-priority-keys))
(evil-define-key 'normal org-mode-map (kbd "M-j") 'org-metadown)
(evil-define-key 'normal org-mode-map (kbd "M-k") 'org-metaup)
(evil-define-key 'normal org-mode-map (kbd "M-l") 'org-metaright)
(evil-define-key 'normal org-mode-map (kbd "M-h") 'org-metaleft)
(evil-define-key 'insert org-mode-map (kbd "M-j") 'org-metadown)
(evil-define-key 'insert org-mode-map (kbd "M-k") 'org-metaup)
(evil-define-key 'insert org-mode-map (kbd "M-l") 'org-metaright)
(evil-define-key 'insert org-mode-map (kbd "M-h") 'org-metaleft)

(defun org-insert-newline-heading ()
  (interactive)
  (evil-append-line())
  (org-insert-heading()))

(defun ozer/new-org-heading (default-enter)
  (if (org-at-heading-p)
      (evil-with-single-undo
        ;; Enter once will make new heading, twice will clear
        (if (eq (org-entry-get nil "ITEM") "")
            (evil-change (line-beginning-position) (line-end-position) 'block ?_)
          ;; Insert a new TODO if we're on a TODO
          (if (org-get-todo-state)
              (org-insert-todo-heading-respect-content)
            (org-insert-heading-respect-content))
          (evil-append 1)))
    ;; Do whatever enter normally does
    (funcall default-enter)))
(evil-define-key 'insert org-mode-map
  (kbd "RET") (lambda (&rest e) (interactive) (ozer/new-org-heading (lambda () (interactive) (evil-org-return e)))))
(evil-define-key 'normal org-mode-map
  (kbd "RET") (lambda () (interactive) (ozer/new-org-heading 'org-open-at-point)))


;; Unsorted ;;
(defun my-org-mode-setup ()
  (progn)
  (evil-define-key 'normal org-mode-map (kbd "RET") (lambda () (interactive) (ozer/new-org-heading 'org-return)))
  ;; This doesn't seem to be applied automatically
  (local-set-key (kbd "C-c a") 'org-agenda)
  (setq org-priority-faces
        `((?A . (:foreground ,(face-attribute 'ansi-color-red :background)))
          (?B . (:foreground ,(face-attribute 'ansi-color-yellow :background)))
          (?C . (:foreground ,(face-attribute 'ansi-color-blue :background)))
          ))
  (company-mode 0)
  (visual-line-mode 1)
  (high-priority-keys))


(add-hook 'org-mode-hook 'my-org-mode-setup)



;; Archive ;;
;; (setq org-modern-checkbox nil)
;; (setq org-modern-table nil)
;; (setq org-modern-todo t)
;; (setq org-appear-autolinks nil)
;; (advice-add 'org-transclusion-propertize-source :override #'tennyson/nop)
;; (add-to-list 'org-transclusion-extensions 'org-transclusion-indent-mode)
