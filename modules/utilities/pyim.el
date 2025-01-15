;;; pyim.el --- Chinese Input Method -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-package pyim
  :bind (:map text-mode-map ("M-j" pyim-convert-string-at-point))
  :custom
  (pyim-default-scheme 'quanpin)
  (pyim-page-tooltip 'posframe)
  (pyim-page-length 5)
  (pyim-directory (no-littering-expand-var-file-name "pyim/"))
  (pyim-dcache-directory (pyim-directory "dcache/")))

(use-package pyim-basedict
  :after (pyim)
  :hook (pyim-mode . pyim-basedict-enable))

(use-package youdao-dictionary
  :commands (youdao-dictionary-search-at-point-posframe)
  :bind (("C-c y" youdao-dictionary-search-at-point-posframe)))

(provide 'irkalla/pyim)
