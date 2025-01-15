;;; vertico.el --- laboratory of Irkalla -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-package vertico
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook ((elpaca-after-init . vertico-mode)
         (rfn-eshadow-update-overlay . vertico-directory-tidy))
  :custom
  (vertico-cycle t)
  (vertico-mouse-mode t))

;; :NOTE| Modify mini-buffer to accomodate Vertico buffers!

(use-feature emacs
  :hook (minibuffer-setup . cursor-intangible-mode)
  :init
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" "" crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  :config
  (setopt enable-recursive-minibuffers t
          minibuffer-prompt-properties
          '(read-only t cursor-intangible t face minibuffer-prompt)))


;; :NOTE| Anontations for Vertico buffers
;; :TODO| add icons to M-x and other buffers akin to =nerd-icons-ivy-rich=!

(use-package marginalia
  :after (vertico)
  :hook (vertico-mode . marginalia-mode)
  :config
  (with-eval-after-load 'nerd-icons-completion
    (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right))

(provide 'irkalla/vertico)
