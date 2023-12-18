;;; init-tab-bar.el --- Built-in Emacs Tab-bar -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Utilizing Emacs built-in tab-bar for easier access to our many tabs.

;;; Code:

(use-package tab-bar
  :elpaca nil
  :hook (elpaca-after-init . tab-bar-mode)
  :bind (("C-<next>" . tab-next)
         ("C-<prior>" . tab-previous))
  :custom
  (tab-bar-show 1)
  (tab-bar-tab-hints t)
  (tab-bar-new-tab-choice "*dashboard*"))

(use-package tabspaces
  :hook (elpaca-after-init . tabspaces-mode)
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-keymap-prefix "C-c p")
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-initialize-project-with-todo t)
  (tabspaces-todo-file-name "project-todo.org")
  (tabspaces-session t)
  :config
  ;; ~Consult-Buffer~ -> filter buffers by workspace
  (with-eval-after-load 'consult
    (consult-customize consult--source-buffer :hidden t :default nil)
    (defvar consult--source-workspace
      (list :name     "Workspace Buffers"
            :narrow   ?w
            :history  'buffer-name-history
            :category 'buffer
            :state    #'consult--buffer-state
            :default  t
            :items    (lambda ()
                        (consult--buffer-query
                         :predicate #'tabspaces--local-buffer-p
                         :sort 'visibility
                         :as #'buffer-name)))

      "Set workspace buffer list for consult-buffer.")
    (add-to-list 'consult-buffer-sources 'consult--source-workspace)))

(provide 'init-tab-bar)
;;; init-tab-bar.el ends here
