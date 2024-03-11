;;; init-eat.el --- Emacs Native Terminal Emulator -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; An Emacs native terminal eumlator for our lispy heads to work with.

;;; Code:

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

;; :NOTE| Setup hydra's for the ever-growing bindings
(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define+ launcher-hydra ()
    ("Terminal"
     (("e" eat         "EAT")
      ("p" eat-project "EAT -> Project")))))

(provide 'init-eat)
;;; init-eat.el ends here
