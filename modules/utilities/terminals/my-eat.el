;;; my-eat.el --- Elisp Based Terminal Emulator -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-package eat
  :ensure (:host codeberg :repo "akib/emacs-eat"
                 :files ("*.el" ("term" "term/*.el") "*.texi"
                         "*.ti" ("terminfo/e" "terminfo/e/*")
                         ("terminfo/65" "terminfo/65/*")
                         ("integration" "integration/*")
                         (:exclude ".dir-locals.el" "*-tests.el")))
  :hook ((eshell-mode . (lambda ()
                          (eat-eshell-mode)
                          (eat-eshell-visual-command-mode))))
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-enable-auto-line-mode t))

(provide 'my-eat)
