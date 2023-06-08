;;; apparatus/init-treemacs.el -*- lexical-binding: t -*-

(defgroup irkalla-treemacs '()
  "A tree-based file and project explorer."
  :tag "Irkalla Treemacs"
  :group 'irkalla)

(use-package treemacs
  :general (irkalla/comma-lead-keydef
             :keymaps 'global-map
             "f"          '(:ignore t                     :which-key "Treemacs")
             "f b"        '(treemacs-bookmark             :which-key "Bookmark file at cursor")
             "f d"        '(treemacs-delete-other-windows :which-key "Delete other Treemacs window")
             "f f"        '(treemacs-find-file            :which-key "Navigate to file in Treemacs")
             "f t"        '(treemacs-find-tag             :which-key "Navigate to tag in Treemacs")
             "f s"        '(treemacs-select-directory     :which-key "Select directory at cursor")
             "f <return>" '(treemacs                      :which-key "(toggle) Tree-based navigation")
             "f <tab>"    '(treemacs-select-window        :which-key "Switch focus to Treemacs if existent"))
  :init (with-eval-after-load 'winum
          (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :custom (treemacs-width 28))

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
