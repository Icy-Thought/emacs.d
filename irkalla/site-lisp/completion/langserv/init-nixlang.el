;;; init-nixlang.el --- Langserv: Nix -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Language server configurations the Nix language.

;;; Code:

(use-package nix-mode
  :mode ("\\.nix\\'" . nix-mode)
  :hook (nix-mode . eglot-ensure)
  :config
  (with-eval-after-load 'eglot
   (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))))

;; :NOTE| apheleia formatting support
(with-eval-after-load 'apheleia
  (when (executable-find "alejandra")
    (push '(alejandra . ("alejandra" "--quiet" "-"))
          apheleia-formatters)
    (setf (alist-get 'nix-mode apheleia-mode-alist)
          '(alejandra))))

(provide 'init-nixlang)
;;; init-nixlang.el ends here
