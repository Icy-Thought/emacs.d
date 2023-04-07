;;; toolset/init-magit.el -*- lexical-binding: t -*-

(defgroup irkalla-magit '()
  "Magical git client for Emacs."
  :tag "Irkalla Magit"
  :group 'irkalla)

;; TODO: expand this later... + add open in new buffer instead of mini-buf.
(use-package magit
  :if (executable-find "git")
  :bind (("C-x g" . magit))
  :custom
  (magit-auto-revert-mode nil)
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

;; Showing our project TODO's in Magit
(use-package magit-todos
  :hook (magit-mode . magit-todos-mode)
  :init
  (setq magit-todos-nice (if (executable-find "nice") t nil)
        magit-todos-scanner #'magit-todos--scan-with-rg)
  :custom
  (magit-todos-recursive t)
  (magit-todos-depth 10)
  (magit-todos-exclude-globs '(".git/" "*.html"))
  :config (custom-set-variables
           '(magit-todos-keywords (list "TODO" "FIXME" "HACK"))))

;; ~git blame~ tool for our setup
(use-package blamer
  :defer t
  :custom
  (blamer-idle-time 0.5)
  (blamer-min-offset 70)
  (blamer-view 'overlay-right)
  (blamer-type 'visual)
  (blamer-max-commit-message-length 70)
  (blamer-force-truncate-long-line nil)
  (blamer-author-formatter " ✎ %s ")
  (blamer-commit-formatter "● \'%s\' ● ")
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :height 125
                   :italic t))))

;; Live highlight of buffer (~git~) changes
(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :diminish git-gutter-mode
  :config (setq git-gutter:update-interval 1))


(provide 'init-magit)
