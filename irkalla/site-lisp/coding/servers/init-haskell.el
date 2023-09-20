;;; init-haskell.el --- Langserv: Haskell -*- lexical-binding: t -*-

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
  (setq eglot-workspace-configuration
        '((haskell (formattingProvider "stylish-haskell")))))

;; :NOTE| apheleia formatting support
(with-eval-after-load 'apheleia
  (when (executable-find "stylish-haskell")
    (push '(stylish-haskell . ("stylish-haskell" "-"))
          apheleia-formatters)
    (setf (alist-get 'haskell-mode apheleia-mode-alist)
          '(stylish-haskell))))

(provide 'init-haskell)
;;; init-haskell.el ends here
