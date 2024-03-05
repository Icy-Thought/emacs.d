;;; init-coding.el --- Coding-related Changes -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Editing within Emacs should become a fluid experience, and completions & LSP help us reduce the amount of typing -> more
;; fluid experience.

;;; Code:

(use-feature emacs
  :custom
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-scroll-output t)
  (compilation-scroll-output t))

(use-package orderless
  :demand t
  :config
  (setopt completion-category-defaults nil
          completion-category-overrides '((file  (styles . (orderless flex)))
                                          (eglot (styles . (orderless flex))))
          completion-styles '(orderless partial-completion basic)
          completions-detailed t
          completion-ignore-case t
          read-buffer-completion-ignore-case t
          read-file-name-completion-ignore-case t))

(use-package breadcrumb
  :hook (prog-mode . breadcrumb-local-mode)
  :custom (breadcrumb-project-max-length -1))

(use-package apheleia
  :diminish apheleia-mode
  :hook (elpaca-after-init . apheleia-global-mode))

(use-package leetcode
  :commands (leetcode)
  :custom
  (leetcode-save-solutions t)
  (leetcode-prefer-language "python3")
  (leetcode-directory (no-littering-expand-var-file-name "leetcode/")))

;; :NOTE| Setup hydra's for the ever-growing bindings
(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define+ editor-hydra ()
    ("Action"
     (("=" apheleia-format-buffer "Buf. Format")))))

;; :NOTE| Import the custom modules
(require 'init-corfu)
(require 'init-tempel)
(require 'init-diagnostics)
(require 'init-debugger)
(require 'init-eldoc)
(require 'init-treesitter)
(require 'init-lsp)

(provide 'init-coding)
;;; init-coding.el ends here
