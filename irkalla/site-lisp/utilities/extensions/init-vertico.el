;;; init-vertico.el --- Vertico: VERTical Interactive COmpletion -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; A performant and minimalistic vertical completion UI based on the default completion system

;;; Code:

(use-package vertico
  :elpaca (:files (:defaults "extensions/*"))
  :requires (vertico-directory)
  :hook (elpaca-after-init . vertico-mode)
  :custom
  (vertico-cycle t)

  (vertico-multiform-categories
   '((file grid reverse)
     (consult-location buffer)
     (consult-grep buffer)
     (minor-mode reverse)
     (imenu buffer)
     (t unobtrusive)))

  (vertico-multiform-commands
   '((consult-dir reverse)
     (execute-extended-command flat)
     (embark-prefix-help-command reverse)
     (completion-at-point reverse))))

(use-package vertico-posframe
  :disabled t
  :hook (vertico-mode . vertico-posframe-mode)
  :custom
  (vertico-posframe-poshandler #'posframe-poshandler-frame-top-center)
  (vertico-posframe-fallback-mode vertico-buffer-mode))

(use-package vertico-mouse
  :elpaca nil
  :hook (vertico-mode . vertico-mouse-mode))

(use-package vertico-directory
  :elpaca nil
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :general (:keymaps 'vertico-map
                     "RET"   #'vertico-directory-enter
                     "DEL"   #'vertico-directory-delete-char
                     "M-DEL" #'vertico-directory-delete-word))

;; :NOTE| Enhances the behavior & appearance of Emacs mini-buffer prompt:
(use-package emacs
  :elpaca nil
  :preface
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" "" crm-separator)
                  (car args))
          (cdr args)))
  :hook (minibuffer-setup . cursor-intangible-mode)
  :config
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setopt minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)))

(provide 'init-vertico)
;;; init-vertico.el ends here
