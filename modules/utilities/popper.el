;;; popper.el --- Scratchpad Like Buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-package popper
  :after (shackle)
  :hook (shackle-mode . popper-mode)
  :custom
  (popper-echo-mode t)
  (popper-display-control nil)
  (popper-echo-dispatch-keys nil)
  (popper-group-function #'popper-group-by-project)
  (popper-reference-buffers
   `(help-mode helpful-mode
     ,(rx "*Messages*")
     ,(rx "Output*" eos)
     ,(rx "*devdocs*")
     ,(rx "*" (* any) "REPL" (* any) "*")
     compilation-mode magit-process-mode
     eat-mode eshell-mode shell-mode term-mode vterm-mode)))

(provide 'irkalla/popper)
