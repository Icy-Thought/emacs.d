;;; python.el --- Python Development Environment -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-package python-mode
  :mode ("\\.py\\'" . python-ts-mode)
  :hook (python-ts-mode . eglot-ensure)
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "--simple-prompt"))

;; :NOTE| Adding a formatting option

(when (executable-find "ruff")
  (with-eval-after-load 'apheleia
    (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff ruff-isort))))

(provide 'irkalla/python)
