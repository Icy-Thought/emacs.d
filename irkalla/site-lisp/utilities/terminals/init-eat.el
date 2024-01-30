;;; init-eat.el --- Emacs Native Terminal Emulator -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; An Emacs native terminal eumlator for our lispy heads to work with.

;;; Code:

(use-package eat
  :elpaca (:host codeberg :repo "akib/emacs-eat"
                 :files ("*.el" ("term" "term/*.el") "*.texi"
                         "*.ti" ("terminfo/e" "terminfo/e/*")
                         ("terminfo/65" "terminfo/65/*")
                         ("integration" "integration/*")
                         (:exclude ".dir-locals.el" "*-tests.el")))
  :hook ((eshell-mode . (lambda ()
                          (eat-eshell-mode) 
                          (eat-eshell-visual-command-mode)))
         (eat-exec . (lambda (&rest _) (turn-off-evil-mode))))
  :custom (eat-kill-buffer-on-exit t))

(provide 'init-eat)
;;; init-eat.el ends here
