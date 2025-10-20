(defun tennyson/display-cols (pdir &rest args)
  (find-file (expand-file-name (car args) pdir))
  (mapcar
   (lambda (arg)
     (progn
       (split-window-right)
       (evil-window-right 1)
       (find-file (expand-file-name arg pdir))
       ))
   (cdr args))
  )

(spacemacs|define-custom-layout "tennyson.ts"
  :binding "t"
  :body
  (let
      ((pdir "~/repos/tennysontbardwell/tennyson.ts/"))
    (progn
      (find-file (expand-file-name "tennyson/app/cli/quickdev.ts" pdir))
      (split-window-right)
      (find-file (expand-file-name "README.md" pdir))
      (split-window-right)
      (vterm "*tennyson.ts-watch*")
      (vterm-send-string "yarn run watch\n")
      (call-interactively 'shell-pop)
      (evil-window-right 1)
      )))

(spacemacs|define-custom-layout "personal.ts"
  :binding "p"
  :body
  (let
      ((pdir "~/repos/tennysontbardwell/misc-projects/personal.ts/"))
    (progn
      (find-file (expand-file-name "README" pdir))
      (split-window-right)
      (find-file (expand-file-name "README" pdir))
      (split-window-right)
      (vterm "*personal.ts-watch*")
      (vterm-send-string "yarn run watch\n")
      (call-interactively 'shell-pop)
      (evil-window-right 1)
      )))

(spacemacs|define-custom-layout "nix"
  :binding "n"
  :body
  (progn
    (tennyson/display-cols
     "~/repos/tennysontbardwell/public/dotfiles/nix/.config/nix/"
     "flake.nix"
     "tools.nix"
     "onyx-config.nix")
    (evil-window-left 1)))
