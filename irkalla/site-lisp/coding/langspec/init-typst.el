;;; init-typst.el --- Langserv: Typst -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Language server configurations for Typst.

;;; Code:

(use-package typst-ts-mode
  :elpaca (:host sourcehut :repo "meow_king/typst-ts-mode")
  :mode ("\\.typ\\'" . typst-ts-mode)
  :hook (typst-ts-mode . eglot-ensure)
  :custom (typst-ts-mode-watch-options "--open")
  :config
  (with-eval-after-load 'eglot
    (when (executable-find "typst-lsp")
      (add-to-list 'eglot-server-programs '(typst-ts-mode . ("typst-lsp")))))

  ;; :NOTE| apheleia formatting support
  (with-eval-after-load 'apheleia-formatters
    (when (executable-find "typstfmt")
      (setf (alist-get 'typstfmt apheleia-formatters) '("typstfmt"))
      (add-to-list 'apheleia-mode-alist '(typst-ts-mode . typstfmt))))

  (with-eval-after-load 'consult-imenu
    (setopt consult-imenu-config
            (append consult-imenu-config '((typst-ts-mode
                                            :topLevel "Headings"
                                            :types ((?h "Headings" typst-ts-markup-header-face)
                                                    (?f "Functions" font-lock-function-name-face))))))))

;; :NOTE| Finally, it's time for us to define our Hydra
(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define typst-hydra
    (:title (pretty-hydra-title "──｢ Langspec: Typst ｣──" 'mdicon "nf-md-math_compass")
            :color teal :quit-key "q")
    ("Build"
     (("c" typst-ts-mode-compile "Compile")
      ("p" typst-ts-mode-preview "Preview")
      ("u" typst-ts-mode-compile-and-preview "Run & Preview")))))

(provide 'init-typst)
;;; init-typst.el ends here
