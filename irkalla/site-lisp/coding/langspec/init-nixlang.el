;;; init-nixlang.el --- Langserv: Nix -*- lexical-binding: t; -*-

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
    (when (executable-find "nil")
      (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))))

  ;; :NOTE| apheleia formatting support
  (with-eval-after-load 'apheleia
    (when (executable-find "alejandra")
      (setf (alist-get 'alejandra apheleia-formatters)
            '("alejandra" "--quiet" "-"))
      (add-to-list 'apheleia-mode-alist '(nix-mode . alejandra)))))

(provide 'init-nixlang)
;;; init-nixlang.el ends here
