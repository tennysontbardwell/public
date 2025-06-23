(defun tennyson/nop ()
  ".")

(defun tennyson/replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))


(defun tennyson/clear-highlight (&rest args)
  (evil-ex-nohighlight))

(advice-add 'evil-insert-state :before #'tennyson/clear-highlight)
(advice-add 'evil-visual-make-region :before #'tennyson/clear-highlight)


(defun tennyson/read-lines (file)
  (with-current-buffer (find-file-noselect file)
    (mapcar (lambda (x) (split-string x " " t))
            (split-string
             (buffer-substring-no-properties (point-min) (point-max))
             "\n"))))

(defun tennyson/favorite-files ()
  (interactive)
  (find-file
   (completing-read
    "Pick file: "
    (tennyson/read-lines "~/.config/tennyson/bookmarks.txt")
    nil t "")
   ))

(defun tennyson/toggle-invisible-search ()
  (interactive)
  (make-local-variable 'search-invisible)
  (setq search-invisible (not search-invisible))
  (if search-invisible
      (progn
        (setq search-invisible 'open)
        (message "Search invisible enabled"))
    (message "Search invisible disabled")))

(defun tennyson/swiper-visible-only ()
  (interactive)
  (setq tmp-search-invisible search-invisible)
  (setq search-invisible nil)
  (swiper)
  (setq search-invisible tmp-search-invisible)
  )

(defun tennyson/uuidgen-4 ()
  "Insert a random UUID.
Example of a UUID: 1df63142-a513-c850-31a3-535fc3520c3d

WARNING: this is a simple implementation. The chance of generating the same UUID is much higher than a robust algorithm.."
  (format "%04x%04x-%04x-%04x-%04x-%06x%06x"
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 6))
          (random (expt 16 6)) ) )
