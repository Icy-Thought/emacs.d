;;; init-typst.el --- Langserv: Typst -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Language server configurations for Typst.

;;; Code:

(use-package typst-mode
  :elpaca (:host github :repo "Ziqi-Yang/typst-mode.el")
  :mode ("\\.typ\\'" . typst-mode))

(provide 'init-typst)
;;; init-typst.el ends here
