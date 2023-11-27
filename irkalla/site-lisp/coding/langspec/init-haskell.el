;;; init-haskell.el --- Langserv: Haskell -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Language server configurations for Haskell.

;;; Code:

(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :hook (haskell-mode . eglot-ensure))

;; :NOTE| apheleia formatting support
(when (executable-find "stylish-haskell")
  (with-eval-after-load 'apheleia
    (setf (alist-get 'stylish-haskell apheleia-formatters)
          '("stylish-haskell" "-"))
    (add-to-list 'apheleia-mode-alist '(haskell-mode . stylish-haskell))))

(provide 'init-haskell)
;;; init-haskell.el ends here
