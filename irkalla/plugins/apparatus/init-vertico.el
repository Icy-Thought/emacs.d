;;; apparatus/init-vertico.el -*- lexical-binding: t -*-

(defgroup irkalla-vertico '()
  "A mini-buffer completion and annotation system."
  :tag "Irkalla Vertico"
  :group 'irkalla)

(use-package vertico
  :elpaca (:files (:defaults "extensions/*"))
  :hook (elpaca-after-init . vertico-mode)
  :custom (vertico-cycle t))

(use-package vertico-mouse
  :elpaca nil
  :hook (vertico-mode . vertico-mouse-mode))

(use-package vertico-directory
  :elpaca nil
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :general (:keymaps 'vertico-map
                     "RET"   'vertico-directory-enter
                     "DEL"   'vertico-directory-delete-char
                     "M-DEL" 'vertico-directory-delete-word))

(use-package emacs
  :elpaca nil
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

;; :NOTE| Marks and annotates minibuffer (vertico) completions
(use-package marginalia
  :hook (elpaca-after-init . marginalia-mode)
  :general (general-nmap
             :keymaps 'minibuffer-local-map
             "M-A"  '(marginalia-cycle :which-key "Cycle between Marginalia annotators"))
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right))

(provide 'init-vertico)
