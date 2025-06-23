(org-link-set-parameters
 "dropbox"
 :follow (lambda (key)
           (org-open-file (concat "/Users/tennyson/Dropbox/" key)))
 )

(org-link-set-parameters
 "tennyson-docs"
 :follow (lambda (key)
           (org-open-file (concat "/Users/tennyson/tennyson/docs/" key)))
 )

(setq projectile-project-root-files-child-of
      '(
        "~/repos/tennysontbardwell/?$"
        ))

(defun projectile-root-child-of (dir &optional list)
  (projectile-locate-dominating-file
   dir
   (lambda (dir)
     (--first
      (if (and
           (s-equals? (file-remote-p it) (file-remote-p dir))
           (string-match-p (expand-file-name it) (expand-file-name dir)))
          dir)
      (or list projectile-project-root-files-child-of (list))))))
