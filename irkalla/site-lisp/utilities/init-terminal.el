;;; init-terminal.el --- Terminal Emulator for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Reducing navigation between windows by providing Emcaas with a terminal eumlator.

;;; Code:

(use-package tramp
  :elpaca nil
  :custom
  (tramp-default-method "ssh")
  (remote-file-name-inhibit-cache nil))

(use-package eat
  :elpaca (:host codeberg :repo "akib/emacs-eat"
                 :files ("*.el" ("term" "term/*.el") "*.texi"
                         "*.ti" ("terminfo/e" "terminfo/e/*")
                         ("terminfo/65" "terminfo/65/*")
                         ("integration" "integration/*")
                         (:exclude ".dir-locals.el" "*-tests.el")))
  :hook ((eshell-mode . (lambda ()
                          (eat-eshell-mode +1)
                          (setopt eshell-visual-commands nil)
                          (eat-eshell-visual-command-mode +1))))
  :general
  (irkalla/space-lead-keydef
    "t"   '(:ignore t :which-key "Terminal")
    "t t" '(eat       :which-key "Open EAT")
    "t e" '(eshell    :which-key "Open Eshell"))
  :custom (eat-kill-buffer-on-exit t))

(irkalla/enable-modules (eshell))

(provide 'init-terminal)
;;; init-terminal.el ends here
