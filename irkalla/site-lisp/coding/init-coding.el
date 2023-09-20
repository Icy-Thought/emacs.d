;;; init-coding.el --- Coding-related Changes -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Editing within Emacs should become a fluid experience, and completions & LSP help us reduce the amount of typing -> more
;; fluid experience.

;;; Code:

;; Editing basis
(irkalla/enable-modules
 (corfu tempel diagnostics apheleia eldoc))

;; Language-specific
(irkalla/enable-modules
 (babel elisp haskell nixlang markdown orgmode org-roam python rust))

(use-package emacs
  :elpaca nil
  :custom
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-scroll-output t)
  (compilation-scroll-output t)

  (completion-ignore-case t)
  (completions-detailed t)

  (read-file-name-completion-ignore-case t)
  (tab-always-indent 'completion)
  (tab-first-completion 'word-or-paren-or-punct))

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles  . (orderless flex)))
                                   (eglot (styles . (orderless flex))))))

(use-package eglot
  :elpaca nil
  :general
  (irkalla/space-lead-keydef
    :states 'normal :keymaps 'eglot-mode-map
    "l a"    '(eglot-code-actions    :which-key "Perform code-actions")
    "l r"    '(eglot-rename          :which-key "Rename $SYMB")
    "l f"    '(eglot-format          :which-key "Format buffer")
    "l ?"    '(xref-find-references  :which-key "Find -> references")
    "l d"    '(xref-find-definitions :which-key "Find -> definition")
    "l /"    '(xref-find-apropos     :which-key "Find $SYMB <- pattern"))
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits nil)
  (eglot-extend-to-xref t)
  (eglot-ignored-server-capabilities '(:documentHighlightProvider))
  (eglot-menu-string "LSP"))

(use-package jinx
  :elpaca nil
  :hook (text-mode . jinx-mode)
  :general
  (:states 'normal :keymaps '(prog-mode-map text-mode-map)
    "z =" '(jinx-correct :which-key "Correct damned misspellings...")))

(provide 'init-coding)
;;; init-coding.el ends here
