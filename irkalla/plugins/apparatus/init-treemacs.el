;;; toolset/init-treemacs.el -*- lexical-binding: t -*-

(defgroup irkalla-vertico '()
  "A tree-based file and project explorer."
  :tag "Irkalla Treemacs"
  :group 'irkalla)

(use-package treemacs
  :bind
  (:map global-map
        ("M-0" . treemacs-select-window)
        ("C-x t 1" . treemacs-delete-other-windows)
        ("C-x t t" . treemacs)
        ("C-x t d" . treemacs-select-directory)
        ("C-x t B" . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  :init (with-eval-after-load 'winum
          (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay 0.5
          treemacs-hide-dot-git-directory t
          treemacs-max-git-entries 5000
          treemacs-no-png-images nil
          treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-litter-directories '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor nil
          treemacs-show-hidden-files t
          treemacs-sorting 'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-tag-follow-delay 1.5
          treemacs-text-scale nil
          treemacs-wide-toggle-width 70
          treemacs-width 35
          treemacs-width-increment 1)

    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil)))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :config (treemacs-set-scope-type 'Tabs))

(provide 'init-treemacs)
