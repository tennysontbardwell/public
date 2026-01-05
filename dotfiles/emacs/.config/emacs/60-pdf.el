(defun my/open-file-externally (file)
  "Open FILE in the OS default external application."
  (pcase system-type
    ('darwin
     (start-process "open-file-externally" nil "open" file))
    ('windows-nt
     ;; Uses the file association.
     (w32-shell-execute "open" file))
    (_
     (start-process "open-file-externally" nil "xdg-open" file))))

(defun my/pdf-visit-dispatch ()
  "Prompt whether to open the visited PDF externally or in Emacs."
  (let ((file buffer-file-name))
    (message "test")
    (when (and file (string-match-p "\\.pdf\\'" file))
      (if (y-or-n-p (format "Open %s in external viewer? "
                            (file-name-nondirectory file)))
          (progn
            (my/open-file-externally file)
            ;; Don’t switch into DocView/PDFView; keep buffer simple.
            (fundamental-mode)
            (setq buffer-read-only t)
            (erase-buffer)
            (insert (format "Opened externally: %s\n\nKill this buffer when done."
                            file))
            (set-buffer-modified-p nil))
        ;; Open in Emacs: prefer pdf-tools if available, else DocView.
        (cond
         ((fboundp 'pdf-view-mode) (pdf-view-mode))
         (t (doc-view-mode)))))))

;; Ensure our rule wins by being earlier in `auto-mode-alist`.
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . my/pdf-visit-dispatch))

(defun my/open-file-externally (file)
  "Open FILE in the OS default external application."
  (pcase system-type
    ('darwin
     (start-process "open-file-externally" nil "open" file))
    ('windows-nt
     (w32-shell-execute "open" file))
    (_
     (start-process "open-file-externally" nil "xdg-open" file))))

(defun my/pdf-file-p (filename)
  (and (stringp filename)
       (message (concat "checking" filename))
       (string-match-p "\\.pdf\\'" (downcase filename))
       ;; Optional: don’t try to externally open TRAMP paths.
       (not (file-remote-p filename))))

(defun my/find-file-noselect--external-pdf (orig filename &rest args)
  "If visiting a PDF interactively, offer to open externally without visiting."
  (let* ((file (and filename (expand-file-name filename))))
    (if (and
         (message "advice called")
         (message "advice called 2")
         ;; (called-interactively-p 'any) ;; This might break macros
         (not executing-kbd-macro)
         (message "advice called 3")
         (my/pdf-file-p file)
         (message "avoid loading")
         (y-or-n-p (format "Open %s externally (avoid loading into Emacs)? "
                           (file-name-nondirectory file))))
        (progn
          ;; If it’s already visited, optionally close it.
          (let ((buf (get-file-buffer file)))
            (when (buffer-live-p buf)
              (when (y-or-n-p "That PDF already has a buffer; kill it? ")
                (kill-buffer buf))))
          (my/open-file-externally file))
      (apply orig filename args))))

(advice-add 'find-file-noselect :around #'my/find-file-noselect--external-pdf)
