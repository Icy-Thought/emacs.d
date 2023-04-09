;;; toolset/init-vertico.el -*- lexical-binding: t -*-

(defgroup irkalla-vertico '()
  "a mini-buffer completion system"
  :tag "Irkalla Vertico"
  :group 'irkalla)

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*"))
  :hook (after-init . vertico-mode)
  :custom (vertico-cycle t))

(use-package vertico-mouse
  :straight nil
  :hook (vertico-mode . vertico-mouse-mode))

(use-package vertico-directory
  :straight nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package emacs
  :straight (:type built-in)
  :init
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" "" crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (setq minibuffer-prompt-properties
	    '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(provide 'init-vertico)