(defun tennyson/org-get-eod-from-subheadings ()
  (interactive)
  (save-restriction
    (save-excursion
      (org-narrow-to-subtree)
      (let* ((tree (org-element-map
                       (org-element-parse-buffer)
                       'headline
                     (lambda (el)
                       (if (eq (org-element-property
                                :level el)
                               4)
                           (list
                            (org-element-property
                             :raw-value el)
                            (org-element-property
                             :raw-value (nth 0 (org-element-property
                                                :title el)))
                            (unwind-protect
                                (substring
                                 (org-element-property
                                  :raw-value el)
                                 23
                                 nil)
                              ""))
                         nil))))
             (tree (seq-filter
                    (lambda (x) (not (eq x nil)))
                    tree))
             (table-rows (mapconcat
                          (lambda (x)
                            (format
                             "| %s | | | %s |"
                             (nth 1 x)
                             (nth 2 x)))
                          tree
                          "\n"))
             (table (format
                     "%s\n%s"
                     table-rows
                     "#+TBLFM: @>$2=0::$2=(date(<@+1$1>)-date(<$1>))*24*60;%.0f::$3='(orgtbl-ascii-draw $2 0 600 30)")))
        (kill-new table)))))


;; https://kagi.com/assistant/385c8795-6558-4053-a5d9-1483b95acbe8
(defun tennyson/firefox-get-tabs ()
  "Fetch and parse tabs from Firefox extension."
  (require 'json)
  (let* ((url-request-method "POST")
         (url-request-data "{\"command\": \"list_tabs\"}")
         (response (url-retrieve-synchronously "http://localhost:9001")))
    (with-current-buffer response
      (goto-char (point-min))
      (re-search-forward "\n\n" nil t)
      (let* ((json-string (buffer-substring-no-properties (point) (point-max)))
             (json-data (json-read-from-string json-string)))
        (kill-buffer response)
        (if (stringp json-data)
            (progn (message "Server error: %s" json-data) nil)
          (append json-data nil))))))

(defun tennyson/firefox-format-tab (obj &optional bullet)
  "Format tab object as org link with optional bullet."
  (let ((url (alist-get 'url obj))
        (title (alist-get 'title obj)))
    (format "%s[[%s][%s]]%s"
            (if bullet "- " "")
            url title
            (if bullet "\n" ""))))

(defun tennyson/firefox-insert-frontmost-link ()
  (interactive)
  (when-let ((tabs (tennyson/firefox-get-tabs))
             (active-tab (cl-find-if (lambda (obj) (eq (alist-get 'active obj) t)) tabs)))
    (insert (tennyson/firefox-format-tab active-tab))))

(defun tennyson/firefox-insert-all-tabs ()
  (interactive)
  (when-let ((tabs (tennyson/firefox-get-tabs)))
    (dolist (tab tabs)
      (insert (tennyson/firefox-format-tab tab t)))))

(defun tennyson/firefox-insert-highlighted-tabs ()
  (interactive)
  (when-let ((tabs (tennyson/firefox-get-tabs)))
    (dolist (tab tabs)
      (when (eq (alist-get 'highlighted tab) t)
        (insert (tennyson/firefox-format-tab tab t))))))

;; In mac.c, removed in Emacs 23.
(declare-function org-mac-link-do-applescript "org-mac-message" (script))
(unless (fboundp 'org-mac-link-do-applescript)
  ;; Need to fake this using shell-command-to-string
  (defun org-mac-link-do-applescript (script)
    (let (start cmd return)
      (while (string-match "\n" script)
        (setq script (replace-match "\r" t t script)))
      (while (string-match "'" script start)
        (setq start (+ 2 (match-beginning 0))
              script (replace-match "\\'" t t script)))
      (setq cmd (concat "osascript -e '" script "'"))
      (setq return (shell-command-to-string cmd))
      (concat "\"" (org-trim return) "\""))))

(defun tennyson/chrome-get-all-links ()
  (interactive)
  (let ((result
         (org-mac-link-do-applescript
          "set res to \"\"\ntell application \"Google Chrome\"\nrepeat with t in tabs of windows\nset res to res & \"- [[\" & URL of t & \"][\" & title of t & \"]]\\n\"\nend repeat\nend tell\nreturn res")))
    (insert (replace-regexp-in-string
             "^\"\\|\"$" "" result))))

(setq org-file-apps
      '(("\\.pdf\\'" . emacs)
        (auto-mode . emacs)
        (directory . emacs)
        ("\\.mm\\'" . default)
        ("\\.x?html?\\'" . default)))

(defun tennyson/chrome-insert-frontmost-link ()
  (interactive)
  (let ((result
         (org-mac-link-do-applescript
          "tell application \"Google Chrome\"\nset t to active tab of first window\nreturn \"[[\" & URL of t & \"][\" & title of t & \"]]\"\nend tell")))
    (insert
     (tennyson/replace-in-string "mail/u/0/#inbox/" "mail/u/0/#all/"
                                 (replace-regexp-in-string
                                  "^\"\\|\"$" "" result)))))


;; According to Carten Dominik in
;; https://lists.gnu.org/archive/html/emacs-orgmode/2009-11/msg01195.html,
;; 'org-id-update-id-locations' searches for IDs in: a) agenda files and
;; archives; b) the files in org-id-extra-files; c) all the files that are
;; currently in the id list; d) any live buffers visiting an org-mode file.
;; Examining 'org-id-update-id-locations' we see that "d)" is not (or no
;; longer) true.  This means it becomes hard to make 'org-id' aware of links
;; that somehow have gotten astray, short of manually adding individual
;; files to 'org-id-extra-files'.  This function just provides a way to
;; rebuild lost ID links from all Org files in a given directory.  It is a
;; costly operation, because every Org file will be scanned, but once the
;; links are back to 'org-id-locations', the regular mechanisms of 'org-id'
;; will work as usual.
(defun my/org-id-update-id-locations-directory (dir &optional silent)
  "Scan all Org files in DIR for IDs."
  (interactive (list
                (read-directory-name "Update Org ID locations in dir: ")
                current-prefix-arg))
  (let* ((org-rgxp "\\(\\.org\\|\\.org_archive\\)\\'")
         (files (directory-files-recursively dir org-rgxp))
         (orig-buffers (buffer-list)))
    (org-id-update-id-locations files silent)
    ;; Clean new buffers created by 'org-id-update-id-locations'.
    (let ((new-buffers (seq-filter
                        (lambda (bf)
                          (string-match-p org-rgxp (buffer-file-name bf)))
                        (seq-difference (buffer-list) orig-buffers))))
      (mapc 'kill-buffer new-buffers))))
