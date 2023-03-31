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
        tab-always-indent 'completion
        tab-first-completion 'word-or-paren-or-punct))

;; Orderless: alternative comp system
(use-package orderless
  :after vertico
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles  . (orderless flex)))
                                        (eglot (styles . (orderless flex))))))

;; Eglot
(use-package eglot
  :custom
  (eglot-extend-to-xref t)
  (eglot-ignored-server-capabilities '(:hoverProvider))
  :hydra (hydra-eglot (:exit t :foreign-keys warn :hint nil)
                      "
                               ╭─────────────────────┐
                              <   Hydra Head: Eglot   >
                               └─────────────────────╯
╭────────────────────────┐╭─────────────────┐╭───────────────┐╭─────────────────────┐
│ Find                   ││ Edit            ││ Format        ││ Manage              │
│────────────────────────││─────────────────││───────────────││─────────────────────│
│ [_d_]: Declaration       ││ [_r_]: Rename     ││ [_=_]: Buffer   ││ [_X_]: Shutdown       │
│ [_i_]: Implementation    ││ [_a_]: Actions    ││ [_]_]: Region   ││ [_R_]: Reconnect      │
│ [_D_]: Type definition   ││                 ││               ││ [_E_]: Event Buffer   │
└────────────────────────╯└─────────────────╯└───────────────╯└─────────────────────╯
                                                               ╭───────────────────┐
                                                               │ [_q_]: Exit Hydra!  │
                                                               └───────────────────╯

"
                      ("a" eglot-code-actions)
                      ("R" eglot-reconnect)
                      ("d" eglot-find-declaration)
                      ("D" eglot-find-typeDefinition)
                      ("E" eglot-events-buffer)
                      ("i" eglot-find-implementation)
                      ("r" eglot-rename)
                      ("X" eglot-shutdown)
                      ("q" nil)
                      ("]" eglot-format)
                      ("=" eglot-format-buffer)))

(provide 'init-completion)
