;;; init-lsp.el --- Language Server Protocol -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; LSP == Swiss Army Knife for programmers!

;;; Code:

(use-feature eglot
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
  :config (fset #'jsonrpc--log-event #'ignore) ;; noticable perf. diff!
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits nil)
  (eglot-events-buffer-size 0) ;; noticable perf. diff!
  (eglot-extend-to-xref t)
  (eglot-ignored-server-capabilities '(:documentHighlightProvider :inlayHintProvider))
  (eglot-menu-string (if (featurep 'nerd-icons) (nerd-icons-faicon "nf-fa-code") "LSP")))

(use-package eglot-booster
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :after (eglot)
  :if (executable-find "emacs-lsp-booster")
  :config (eglot-booster-mode +1))

;; :NOTE| Setup hydra's for the ever-growing bindings
(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define+ editor-hydra ()
    ("Control"
     (("l" (if (eglot-managed-p)
               (eglot-hydra/body)
             (message "You are not in an Eglot buffer.")) "Eglot (LSP)")))))

;; :NOTE| Soluving math equations with different syntaxes!
(require 'init-babel)
(require 'init-elisp)
(require 'init-haskell)
(require 'init-rust)
(require 'init-nix)
(require 'init-markdown)
(require 'init-org)
(require 'init-lua)
(require 'init-python)
(require 'init-typst)
(require 'init-zig)

(provide 'init-lsp)
;;; init-lsp.el ends here
