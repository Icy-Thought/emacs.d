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
  (compilation-scroll-output t))

(use-package orderless
  :demand t
  :custom
  (completion-category-defaults nil)
  (completion-category-overrides '((file  (styles . (orderless flex)))
                                   (eglot (styles . (orderless flex)))))
  (completion-styles '(orderless partial-completion basic))
  (completions-detailed t)
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t))

(use-package eglot
  :elpaca nil
  :pretty-hydra
  ((:title (pretty-hydra-title "──｢ Coding: Eglot ｣──" 'faicon "nf-fa-code")
           :color teal :quit-key "q")
   ("Actions"
    (("a" eglot-code-actions    "Perform code-actions")
     ("r" eglot-rename          "Rename $SYMB"))
    "Look-up"
    (("?" xref-find-references  "Find -> references")
     ("f" xref-find-definitions "Find -> definition")
     ("/" xref-find-apropos     "Find $SYMB <- pattern"))))
  :config (fset #'jsonrpc--log-event #'ignore)
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits nil)
  (eglot-events-buffer-size 0)
  (eglot-extend-to-xref t)
  (eglot-ignored-server-capabilities '(:documentHighlightProvider
                                       :inlayHintProvider))
  (eglot-menu-string (if (featurep 'nerd-icons) (nerd-icons-faicon "nf-fa-code") "LSP")))

(use-package eglot-booster
  :elpaca (:host github :repo "jdtsmith/eglot-booster")
  :after (eglot)
  :if (executable-find "emacs-lsp-booster")
  :config (eglot-booster-mode +1))

(use-package breadcrumb
  :hook (prog-mode . breadcrumb-local-mode)
  :custom (breadcrumb-project-max-length -1))

(use-package apheleia
  :diminish apheleia-mode
  :hook (elpaca-after-init . apheleia-global-mode))

(use-package jinx
  :elpaca nil
  :hook (text-mode . jinx-mode)
  :bind ([remap ispell-word] . jinx-correct))

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
     (("=" apheleia-format-buffer "Buf. Format"))
     "Control"
     (("l" (if (eglot-managed-p)
               (eglot-hydra/body)
             (message "You are not in an Eglot buffer.")) "Eglot (LSP)")))))

;; :NOTE| Import the custom modules
(irkalla/enable-modules
 (corfu tempel diagnostics debugger eldoc treesitter))

(irkalla/enable-modules
 (babel elisp haskell nix rust markdown org lua python typst))

(provide 'init-coding)
;;; init-coding.el ends here
