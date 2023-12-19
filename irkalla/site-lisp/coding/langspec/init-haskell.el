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
  :hook (haskell-mode . eglot-ensure)
  :config
  ;; :NOTE| apheleia formatting support
  (with-eval-after-load 'apheleia
    (when (executable-find "stylish-haskell")
      (setf (alist-get 'stylish-haskell apheleia-formatters)
            '("stylish-haskell" "-"))
      (add-to-list 'apheleia-mode-alist '(haskell-mode . stylish-haskell)))))

(provide 'init-haskell)
;;; init-haskell.el ends here
