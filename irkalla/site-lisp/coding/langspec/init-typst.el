;;; init-typst.el --- Langserv: Typst -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Language server configurations for Typst.

;;; Code:

(use-package typst-mode
  :elpaca (:host sourcehut :repo "meow_king/typst-ts-mode")
  :mode ("\\.typ\\'" . typst-ts-mode)
  :hook (tyspt-ts-mode . eglot-ensure)
  :custom (typst-ts-mode-watch-options "--open")
  :config
  (when (executable-find "typst-lsp")
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs '(typst-ts-mode . ("typst-lsp")))))

  (with-eval-after-load 'consult
    (setopt consult-imenu-config
            (append consult-imenu-config '((typst-ts-mode
                                            :topLevel "Headings"
                                            :types ((?h "Headings" typst-ts-markup-header-face)
                                                    (?f "Functions" font-lock-function-name-face))))))))

(provide 'init-typst)
;;; init-typst.el ends here
