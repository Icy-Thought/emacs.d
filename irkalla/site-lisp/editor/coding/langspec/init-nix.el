;;; init-nix.el --- Langserv: Nix -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Language server configurations the Nix language.

;;; Code:

(use-package nix-ts-mode
  :mode ("\\.nix\\'" . nix-ts-mode)
  :hook (nix-ts-mode . eglot-ensure)
  :config
  (with-eval-after-load 'eglot
    (when (executable-find "nil")
      (add-to-list 'eglot-server-programs '(nix-ts-mode . ("nil")))))

  ;; :NOTE| apheleia formatting support
  (with-eval-after-load 'apheleia-formatters
    (when (executable-find "alejandra")
      (setf (alist-get 'alejandra apheleia-formatters)
            '("alejandra" "--quiet" "-"))
      (add-to-list 'apheleia-mode-alist '(nix-ts-mode . alejandra)))))

(provide 'init-nix)
;;; init-nix.el ends here
