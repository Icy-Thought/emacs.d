;;; init-python.el --- Langserv: Python -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Language server configurations for Python.

;;; Code:

(use-package python-mode
  :mode ("\\.py\\'" . python-mode)
  :hook ((python-mode python-ts-mode) . eglot-ensure)
  :flymake-hook
  ((python-mode python-ts-mode)
   flymake-collection-mypy
   flymake-collection-ruff))

;; :NOTE| apheleia formatting support
(with-eval-after-load 'apheleia
  (when (executable-find "isort")
    (push '(isort . ("isort" "--profile" "black" "--stdout" "-"))
          apheleia-formatters)
    (setf (alist-get 'python-mode apheleia-mode-alist)
          '(isort black))))

(provide 'init-python)
;;; init-python.el ends here
