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
  :preface
  (defun eglot-python-setup ()
    (with-eval-after-load 'eglot  
      (when (executable-find "pylyzer")
        (add-to-list 'eglot-server-programs
                     `((python-mode python-ts-mode) . ("pylyzer" "--server" :initializationOptions ( :diagnostics t ;; " " fixes broken formatting
                                                                                                     :inlineHints t
                                                                                                     :smartCompletion t)))))

      (when (executable-find "pyright-langserver")
        (add-to-list 'eglot-server-programs
                     `((python-mode python-ts-mode) . ("pyright-langserver" "--stdio" :initializationOptions (:pyright (:typeCheckingMode "strict")))))))
    (eglot-ensure))
  :hook ((python-mode python-ts-mode) . eglot-python-setup))

(with-eval-after-load 'apheleia
  (when (executable-find "isort")
    (setf (alist-get 'isort apheleia-formatters)
          '("isort" "--profile black" "--stdout"))
    (add-to-list 'apheleia-mode-alist '((python-mode python-ts-mode) . isort)))

  (when (executable-find "black")
    (setf (alist-get 'black apheleia-formatters)
          '("black" "-"))
    (add-to-list 'apheleia-mode-alist '((python-mode python-ts-mode) . black))))

(provide 'init-python)
;;; init-python.el ends here
