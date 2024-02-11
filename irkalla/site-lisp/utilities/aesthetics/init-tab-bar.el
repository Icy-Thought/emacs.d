;;; init-tab-bar.el --- Built-in Emacs Tab-bar -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Utilizing Emacs built-in tab-bar for easier access to our many tabs.

;;; Code:

(use-package tab-bar
  :ensure nil
  :hook (elpaca-after-init . tab-bar-mode)
  :bind (("C-<next>" . tab-next)
         ("C-<prior>" . tab-previous))
  :custom
  (tab-bar-show 1)
  (tab-bar-tab-hints t)
  (tab-bar-new-tab-choice "*dashboard*"))

(provide 'init-tab-bar)
;;; init-tab-bar.el ends here
