;;; zig.el --- Zig Development Environment -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-package zig-mode
  :mode ("\\.zig\\'" . zig-ts-mode)
  :hook (zig-ts-mode . eglot-ensure)
  :init (derived-mode-add-parents 'zig-ts-mode '(zig-mode)))

;; :NOTE| Adding a formatting option

(when (executable-find "zig")
  (with-eval-after-load 'apheleia
    (setf (alist-get 'zig-ts-mode apheleia-mode-alist) '(zig-fmt))))

(provide 'irkalla/zig)
