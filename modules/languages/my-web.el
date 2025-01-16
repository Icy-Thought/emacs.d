;;; my-web.el --- Web Development Environment -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-feature html-ts-mode
  :mode ("\\.html\\'" . html-ts-mode)
  :custom (html-ts-mode-indent-offset tab-width))

;; :TODO| add biome as a second LSP server when Eglot supports multiple LSPs.

(use-feature typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :hook (typescript-ts-mode . eglot-ensure)
  :custom (typescript-ts-mode-indent-offset tab-width))

;; :NOTE| Adding a formatting option

(when (executable-find "biome")
  (with-eval-after-load 'apheleia
    (setf (alist-get 'biome apheleia-formatters)
          '("biome" "format" "--stdin-file-path" filepath))
    (setf (alist-get typescript-ts-mode apheleia-mode-alist) '(biome))))

(provide 'my-web)
