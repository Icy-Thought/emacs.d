;;; toolset/init-treemacs.el -*- lexical-binding: t -*-

(defgroup irkalla-treemacs '()
  "A tree-based file and project explorer."
  :tag "Irkalla Treemacs"
  :group 'irkalla)

(use-package treemacs
  :bind (:map global-map
              ("M-0" . treemacs-select-window)
              ("C-x t 1" . treemacs-delete-other-windows)
              ("C-x t t" . treemacs)
              ("C-x t d" . treemacs-select-directory)
              ("C-x t B" . treemacs-bookmark)
              ("C-x t C-t" . treemacs-find-file)
              ("C-x t M-t" . treemacs-find-tag))
  :init (with-eval-after-load 'winum
          (define-key winum-keymap (kbd "M-0") #'treemacs-select-window)))

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
