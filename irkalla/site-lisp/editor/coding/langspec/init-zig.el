;;; init-zig.el --- Langserv: Zig -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Language server configurations for the Zig programming language.

;;; Code:

(use-package zig-mode
  :mode ("\\.zig$" . zig-mode)
  :preface
  (defun eglot-zig-setup ()
    (with-eval-after-load 'eglot
      (when (executable-find "zls")
        (add-to-list 'eglot-server-programs `((zig-mode zig-ts-mode) . ("zls")))))
    (eglot-ensure))
  :hook ((zig-mode zig-ts-mode) . eglot-zig-setup))

(with-eval-after-load 'apheleia
  (when (executable-find "zig")
    (setf (alist-get 'zigfmt apheleia-formatters)
          '("zig" "fmt" "--stdin"))
    (add-to-list 'apheleia-mode-alist '((zig-mode zig-ts-mode) . zigfmt))))

(provide 'init-zig)
;;; init-zig.el ends here
