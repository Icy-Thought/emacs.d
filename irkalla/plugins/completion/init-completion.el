;;; completion/init-completion.el -*- lexical-binding: t -*-

;; Require custom completion modules
(require 'init-corfu)
(require 'init-tempel)

;; Emacs-related
(use-package emacs
  :ensure nil
  :init
  (setq completions-detailed t
        completion-ignore-case t
        completion-category-overrides '((eglot (styles orderless)))
        tab-always-indent 'completion
        tab-first-completion 'word-or-paren-or-punct))

;; Orderless: alternative comp system
(use-package orderless
  :init
  (setq completion-styles '(substring orderless basic)
	    completion-category-defaults nil
	    completion-category-overrides '((file (styles . (partial-completion))))))

;; Eglot
(use-package eglot
  :after hydra
  :custom (eglot-extend-to-xref t)
  :hydra (hydra-eglot (:exit t :foreign-keys warn :hint nil)
"
╭────────────────────────┐╭─────────────────┐╭───────────────┐╭─────────────────────┐
│ Find                   ││ Edit            ││ Format        ││ Manage              │
│────────────────────────││─────────────────││───────────────││─────────────────────│
│ [_d_]: Declaration     ││ [_r_]: Rename   ││ [_=_]: Buffer ││ [_X_]: Shutdown     │
│ [_i_]: Implementation  ││ [_a_]: Actions  ││ [_R_]: Region ││ [_C_]: Reconnect    │
│ [_D_]: Type definition ││                 ││               ││ [_E_]: Event Buffer │
└────────────────────────╯└─────────────────╯└───────────────╯└─────────────────────╯
      ╭──────────────────┐╭───────────────────┐╭──────────────────────────────┐
      │ [_X_]: Shutdown  ││ [_C_]: Re-connect ││ [_E_]: Display Events Buffer │
      └──────────────────╯└───────────────────╯└──────────────────────────────╯
"
              ("d" eglot-find-declaration)
              ("i" eglot-find-implementation)
              ("D" eglot-find-typeDefinition)
              ("r" eglot-rename)
              ("a" eglot-code-actions)
              ("=" eglot-format-buffer)
              ("R" eglot-format)
              ("X" eglot-shutdown)
              ("C" eglot-reconnect)
              ("E" eglot-events-buffer)))

(provide 'init-completion)
