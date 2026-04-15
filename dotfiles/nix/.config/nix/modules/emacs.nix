{ pkgs, ... }:
let
  my-emacs = pkgs.emacs.override {
    # withNS = false;
    withPgtk = true;
    # withXwidgets = true;
    withWebP = true;
    # withNativeCompilation = true;
    # withSQLite3 = true;
    # withTreeSitter = true;
  };
in
{
  paths = with pkgs; [
    ((emacsPackagesFor my-emacs).emacsWithPackages (
      epkgs: 
          builtins.filter (p: (p.pname or "") != "pcomplete")
        (with epkgs; [
        mu4e

        # "info+" BROKEN
        # chatgpt
        # codegpt
        # dall-e
        # drupal-mode
        # eaf
        # evil-evilified-state
        # evil-unimpaired
        # hide-comnt
        # holy-mode
        # hybrid-mode
        # hyperbole
        # php-auto-yasnippets
        # php-extras
        # pylookup
        # space-doc
        # spacemacs-purpose-popwin
        # spacemacs-whitespace-cleanup
        # term-cursor
        # vim-powerline
        # zonokai-emacs
        # majapahit-themes

        # afternoon-theme
        # alect-themes
        # ample-theme
        # ample-zen-theme
        # anti-zenburn-theme
        # apropospriate-theme
        # badwolf-theme
        # birds-of-paradise-plus-theme
        # bubbleberry-theme
        # busybee-theme
        # cherry-blossom-theme
        # chocolate-theme
        # clues-theme
        # color-theme-sanityinc-solarized
        # color-theme-sanityinc-tomorrow
        # cyberpunk-theme
        # dakrone-theme
        # darkmine-theme
        # darkokai-theme
        # darktooth-theme
        # django-theme
        # doom-themes
        # dracula-theme
        # ef-themes
        # espresso-theme
        # exotica-theme
        # eziam-themes
        # farmhouse-themes
        # flatland-theme
        # flatui-theme
        # gandalf-theme
        # gotham-theme
        # grandshell-theme
        # gruber-darker-theme
        # gruvbox-theme
        # hc-zenburn-theme
        # hemisu-theme
        # heroku-theme
        # inkpot-theme
        # ir-black-theme
        # jazz-theme
        # jbeans-theme
        # kaolin-themes
        # light-soap-theme
        # lush-theme
        # madhat2r-theme
        # material-theme
        # minimal-theme
        # modus-themes
        # moe-theme
        # molokai-theme
        # monochrome-theme
        # monokai-theme
        # mustang-theme
        # naquadah-theme
        # noctilux-theme
        # obsidian-theme
        # occidental-theme
        # oldlace-theme
        # omtose-phellack-themes
        # organic-green-theme
        # phoenix-dark-mono-theme
        # phoenix-dark-pink-theme
        # planet-theme
        # professional-theme
        # purple-haze-theme
        # railscasts-theme
        # rebecca-theme
        # reverse-theme
        # seti-theme
        # smyx-theme
        # soft-charcoal-theme
        # soft-morning-theme
        # soft-stone-theme
        # solarized-theme
        # soothe-theme
        # spacegray-theme
        # subatomic-theme
        # subatomic256-theme
        # sublime-themes
        # sunny-day-theme
        # tango-2-theme
        # tango-plus-theme
        # tangotango-theme
        # tao-theme
        # timu-macos-theme
        # toxi-theme
        # twilight-anti-bright-theme
        # twilight-bright-theme
        # twilight-theme
        # ujelly-theme
        # underwater-theme
        # white-sand-theme
        # zen-and-art-theme
        # zenburn-theme

        ace-link
        activity-watch-mode
        aggressive-indent
        ahk-mode
        all-the-icons
        anaphora
        auctex-latexmk
        auto-compile
        auto-dictionary
        auto-highlight-symbol
        auto-yasnippet
        blacken
        bm
        browse-at-remote
        bundler
        centered-cursor-mode
        chruby
        clean-aindent-mode
        code-cells
        code-review
        column-enforce-mode
        command-log-mode
        company-anaconda
        company-auctex
        company-emoji
        company-lua
        company-math
        company-nixos-options
        company-php
        company-phpactor
        company-reftex
        company-terraform
        company-web
        counsel-css
        counsel-gtags
        counsel-projectile
        csv-mode
        cython-mode
        dactyl-mode
        dap-mode
        define-word
        devdocs
        diff-hl
        diminish
        dired-quick-sort
        dired-subtree
        disable-mouse
        dotenv-mode
        drag-stuff
        dumb-jump
        eat
        ebib
        ein
        elfeed-goodies
        elfeed-org
        elisp-def
        elisp-demos
        elisp-slime-nav
        ellama
        emmet-mode
        emoji-cheat-sheet-plus
        emr
        esh-help
        eshell-prompt-extras
        eshell-z
        ess
        ess-R-data-view
        eval-sexp-fu
        evil-anzu
        evil-args
        evil-cleverparens
        evil-collection
        evil-easymotion
        evil-escape
        evil-exchange
        evil-goggles
        evil-iedit-state
        evil-indent-plus
        evil-ledger
        evil-lion
        evil-lisp-state
        evil-lispy
        evil-matchit
        evil-mc
        evil-nerd-commenter
        evil-numbers
        evil-org
        evil-surround
        evil-tex
        evil-textobj-line
        evil-tutor
        evil-visual-mark-mode
        evil-visualstar
        exec-path-from-shell
        expand-region
        eyebrowse
        fancy-battery
        flx-ido
        flycheck-elsa
        flycheck-ledger
        flycheck-package
        flycheck-pos-tip
        flyspell-correct-ivy
        fold-this
        geben
        ggtags
        gh-md
        git-link
        git-messenger
        git-modes
        git-timemachine
        gitignore-templates
        gnuplot
        golden-ratio
        google-translate
        gptel
        graphviz-dot-mode
        hackernews
        helm
        helm-core
        helm-make
        highlight-indentation
        highlight-numbers
        highlight-parentheses
        hl-todo
        hledger-mode
        hungry-delete
        impatient-mode
        importmagic
        indent-guide
        inspector
        ivy-avy
        ivy-bibtex
        ivy-hydra
        ivy-posframe
        ivy-purpose
        ivy-xref
        ivy-yasnippet
        journalctl-mode
        js-doc
        js2-refactor
        json-mode
        json-navigator
        json-reformat
        keycast
        live-py-mode
        livid-mode
        load-relative
        loc-changes
        lorem-ipsum
        lsp-ivy
        lsp-latex
        lsp-origami
        lsp-pyright
        lua-mode
        macrostep
        magit-lfs
        markdown-toc
        minitest
        multi-line
        multi-term
        multi-vterm
        mwim
        nameless
        nix-mode
        nodejs-repl
        nov
        npm-mode
        open-junk-file
        org-cliplink
        org-contrib
        org-download
        org-mac-link
        org-mime
        org-pomodoro
        org-present
        org-projectile
        org-re-reveal
        org-ref
        org-rich-yank
        org-superstar
        orgit-forge
        osm
        overseer
        ox-reveal
        ox-twbs
        pandoc-mode
        paradox
        password-generator
        pdf-view-restore
        phpunit
        pip-requirements
        pipenv
        pippel
        poetry
        polymode
        prettier-js
        pug-mode
        pulsar
        py-isort
        pydoc
        pyenv-mode
        pytest
        python-pytest
        quickrun
        rainbow-delimiters
        rake
        ranger
        rbenv
        realgud
        restart-emacs
        rjsx-mode
        robe
        rspec-mode
        rubocop
        rubocopfmt
        ruby-hash-syntax
        ruby-refactor
        ruby-test-mode
        ruby-tools
        rvm
        sass-mode
        scss-mode
        seeing-is-believing
        shell-pop
        slim-mode
        smeargle
        smex
        spaceline
        sphinx-doc
        spray
        string-edit-at-point
        symbol-overlay
        symon
        systemd
        tagedit
        terminal-here
        test-simple
        tide
        toc-org
        toml-mode
        treemacs-evil
        treemacs-icons-dired
        treemacs-magit
        treemacs-persp
        treemacs-projectile
        typescript-mode
        undo-fu-session
        unfill
        vi-tilde-fringe
        vimrc-mode
        volatile-highlights
        vundo
        web-beautify
        web-mode
        websocket
        wfnames
        wgrep
        winum
        writeroom-mode
        ws-butler
        yaml-mode
        yapfify
        yasnippet-snippets
        zotra
      ])
    ))
  ];
}
