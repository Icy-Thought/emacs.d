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
  :preface
  (defun irkalla/rename-eat-buf ()
    "A simlpe solution which allow the user to spawn multilpe eshells."
    (let ((directory-name (file-name-nondirectory (eshell/pwd))))
      (rename-buffer (concat "*eat-" directory-name "*") t)))
  :hook (eshell-mode . irkalla/rename-eshell-buf)
  :hook ((eat-mode . irkalla/rename-eat-buf)
         (eshell-mode . (lambda ()
                          (eat-eshell-mode +1)
                          (setopt eshell-visual-commands nil)
                          (eat-eshell-visual-command-mode +1))))
  :general
  (irkalla/space-lead-keydef
    "t t" '(eat       :which-key "Open EAT"))
  :custom (eat-kill-buffer-on-exit t))

(provide 'init-eat)
;;; init-eat.el ends here
