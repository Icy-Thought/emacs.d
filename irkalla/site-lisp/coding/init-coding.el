;;; init-coding.el --- Coding-related Changes -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Editing within Emacs should become a fluid experience, and completions & LSP help us reduce the amount of typing -> more
;; fluid experience.

;;; Code:

(use-package emacs
  :elpaca nil
  :custom
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-scroll-output t)
  (compilation-scroll-output t)
  (tab-always-indent 'completion)
  (tab-first-completion 'word-or-paren-or-punct))

(use-package orderless
  :demand t
  :custom
  (completion-category-defaults nil)
  (completion-category-overrides '((file  (styles  . (orderless flex)))
                                   (eglot (styles . (orderless flex)))))
  (completion-styles '(orderless partial-completion basic))
  (completions-detailed t)
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t))

(use-package eglot
  :elpaca nil
  :pretty-hydra
  ((:title (pretty-hydra-title "──｢ Coding: Eglot ｣──" 'devicon "nf-dev-code")
           :color teal :quit-key "q")
   ("Actions"
    (("a" eglot-code-actions    "Perform code-actions")
     ("r" eglot-rename          "Rename $SYMB")
     ("f" eglot-format          "Format buffer"))
    "Look-up"
    (("?" xref-find-references  "Find -> references")
     ("d" xref-find-definitions "Find -> definition")
     ("/" xref-find-apropos     "Find $SYMB <- pattern"))))
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits nil)
  (eglot-extend-to-xref t)
  (eglot-ignored-server-capabilities '(:documentHighlightProvider))
  (eglot-menu-string "LSP"))

(use-package apheleia
  :diminish apheleia-mode
  :hook (elpaca-after-init . apheleia-global-mode))

(use-package jinx
  :elpaca nil
  :hook (text-mode . jinx-mode))

;; :NOTE| Lastly, import our custom modules
(irkalla/enable-modules
 (corfu tempel flymake eldoc treesitter))

(irkalla/enable-modules
 (babel elisp haskell nix rust markdown org lua python typst))

;; :NOTE| Lastly, setup hydra's for our ever-growing bindings
(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define+ main-hydra ()
    ("Coding"
     (("l" (if (eglot-managed-p)
               eglot-hydra/body "Eglot (LSP)"))
      ("c" combobulate-hydra/body "Combobulate")
      ("r" (if (memq major-mode '(rust-mode rust-ts-mode))
               rust-hydra/body "Rust"))
      ("o" (if (eq major-mode 'org-mode)
               org-hydra/body "Org-Mode"))
      ("m" (if (eq major-mode 'markdown-mode)
               markdown-hydra/body "Markdown"))
      ("t" (if (eq major-mode 'typst-ts-mode)
               typst-hydra/body "Typst"))
      ("=" jinx-correct "Spell-Check")))))

(provide 'init-coding)
;;; init-coding.el ends here
