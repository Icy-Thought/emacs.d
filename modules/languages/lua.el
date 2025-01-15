;;; lua.el --- Lua Development Environment -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-package lua-mode
  :mode ("\\.lua\\'" . lua-ts-mode)
  :hook (lua-ts-mode . eglot-ensure))

;; :NOTE| Adding a formatting option

(when (executable-find "stylua")
  (with-eval-after-load 'apheleia
    (setf (alist-get 'lua-ts-mode apheleia-mode-alist) '(stylua))))

(provide 'irkalla/lua)
