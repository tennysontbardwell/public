(defun tennyson/display-cols (pdir &rest args)
  (set-window-dedicated-p (selected-window) nil)
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

(spacemacs|define-custom-layout "personal"
  :binding "p"
  :body
  (let
      ((pdir "~/repos/tennysontbardwell/personal/"))
    (tennyson/display-cols
     pdir
     (format-time-string "log/%G/%G-q%q-w%V.org"))
    (set-window-dedicated-p (selected-window) t)
    ))

(spacemacs|define-custom-layout "tennyson.ts"
  :binding "tt"
  :body
  (let
      ((pdir "~/repos/tennysontbardwell/tennyson.ts/"))
    (progn
      (set-window-dedicated-p (selected-window) nil)
      (find-file (expand-file-name "tennyson/app/cli/quickdev.ts" pdir))
      (split-window-right)
      (find-file (expand-file-name "README.md" pdir))
      (split-window-right)
      (vterm "*tennyson.ts-watch*")
      (vterm-send-string "yarn task watch\n")
      (let ((default-directory pdir))
        (call-interactively 'spacemacs/default-pop-shell))
      (evil-window-right 1)
      )))

(spacemacs|define-custom-layout "personal.ts"
  :binding "tp"
  :body
  (let
      ((pdir "~/repos/tennysontbardwell/misc-projects/personal.ts/"))
    (progn
      (set-window-dedicated-p (selected-window) nil)
      (find-file (expand-file-name "README" pdir))
      (split-window-right)
      (find-file (expand-file-name "README" pdir))
      (split-window-right)
      (vterm "*personal.ts-watch*")
      (vterm-send-string "yarn task watch\n")
      (let ((default-directory pdir))
        (call-interactively 'spacemacs/default-pop-shell))
      (evil-window-right 1)
      )))

(spacemacs|define-custom-layout "nix"
  :binding "n"
  :body
  (let ((pdir "~/repos/tennysontbardwell/public/dotfiles/nix/.config/nix/"))
    (tennyson/display-cols
     pdir
     "flake.nix"
     "modules/tools.nix"
     "hosts/onyx-config.nix")
    (evil-window-left 1)
    (let ((default-directory pdir))
      (call-interactively 'spacemacs/default-pop-shell))
    (evil-window-up 1)
    ))
