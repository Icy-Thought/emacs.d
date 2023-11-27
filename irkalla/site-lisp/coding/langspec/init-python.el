;;; init-python.el --- Langserv: Python -*- lexical-binding: t; -*-

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
  :config
  (when (executable-find "pyright-langserver")
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   '((python-mode python-ts-mode)
                     . ("pyright-langserver" "--stdio"
                        :initializationOptions ((:pyright (:typeCheckingMode "strict")))))))))

;; :NOTE| apheleia formatting support
(when (and (executable-find "black") (executable-find "isort"))
  (with-eval-after-load 'apheleia
    (setf (alist-get 'isort apheleia-formatters)
          '("isort" "--profile" "black" "--stdout" "-"))
    (add-to-list 'apheleia-mode-alist '((python-mode python-ts-mode) . isort))
    (add-to-list 'apheleia-mode-alist '((python-mode python-ts-mode) . black))))

(provide 'init-python)
;;; init-python.el ends here
