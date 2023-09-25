;;; init-frames.el --- Manipulation of Emacs Frames -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Controlling the Frames of our Emacs instance, zooming in and whatnot.

;;; Code:

(use-package emacs
  :elpaca nil
  :custom
  (backward-delete-char-untabify-method 'hungry)
  (cursor-in-non-selected-windows nil)
  (dired-kill-when-opening-new-dired-buffer t)
  (echo-keystrokes 0.02)
  (enable-recursive-minibuffers t)
  (help-window-select t)
  (read-buffer-completion-ignore-case t)
  (read-process-output-max (* 64 1024));
  (ring-bell-function 'ignore)
  (debug-on-error init-file-debug)
  (jka-compr-verbose init-file-debug))

(use-package tab-bar
  :elpaca nil
  :hook (elpaca-after-init . tab-bar-mode)
  :bind (("C-<next>" . tab-next)
         ("C-<prior>" . tab-previous))
  :custom (tab-bar-tab-hints t))

(use-package tabspaces
  :hook (tab-bar-mode . tabspaces-mode)
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
           :items    (lambda () (consult--buffer-query
                                 :predicate #'tabspaces--local-buffer-p
                                 :sort 'visibility
                                 :as #'buffer-name)))

     "Set workspace buffer list for consult-buffer.")
   (add-to-list 'consult-buffer-sources 'consult--source-workspace)))

(use-package posframe
  :custom (posframe-mouse-banish '(0 . 5000)))

(use-package winner
  :elpaca nil
  :hook (elpaca-after-init . winner-mode)
  :general
  (irkalla/comma-lead-keydef
   "w"   '(:ignore t            :which-key "Winner Mode")
   "w p" '(delete-other-windows :which-key "Win -> Zoom-in")
   "w u" '(winner-undo          :which-key "Undo Winner")
   "w r" '(winner-redo          :which-key "Redo Winner")))

(provide 'init-frames)
;;; init-frames.el ends here
