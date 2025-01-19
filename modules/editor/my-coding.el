;;; my-coding.el --- Creating A Development Environment -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-feature emacs
  :hook (compilation-filter . ansi-color-compilation-filter)
  :config
  (setopt tab-always-indent 'complete
          compilation-scroll-output t
          compilation-skip-visited t
          compilation-always-kill t
          read-buffer-completion-ignore-case t
          read-file-name-completion-ignore-case t
          read-extended-command-predicate #'command-completion-default-include-p))

;; :NOTE| Emacs built-in Language server protocol

(use-feature eglot
  :defer t
  :config (advice-add 'jsonrpc--log-event :override #'ignore)
  :custom
  (eglot-menu-string "</>")
  (eglot-autoshutdown t)
  (eglot-sync-connect 0)
  (eglot-extend-to-xref t)
  (eglot-report-progress nil)
  (eglot-confirm-server-initiated-edits nil)
  (eglot-ignored-server-capabilities '(:documentHighlightProvider))
  (eglot-events-buffer-config '(:size 0)))

;; :NOTE| Boosting Eglot's ego to make it faster

(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster")
  :if (executable-find "emacs-lsp-booster")
  :after (eglot)
  :config (eglot-booster-mode)
  :custom (eglot-booster-io-only t))

;; :NOTE| Complation candidate matching

(use-package orderless
  :after (corfu)
  :custom
  (completions-detailed t)
  (completion-ignore-case t)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; :NOTE| Documentation of a given $SYMB

(use-feature eldoc
  :custom
  (eldoc-idle-delay 1.0)
  (eldoc-echo-area-display-truncation-message nil)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly))

(use-package eldoc-box
  :commands (eldoc-box-help-at-point)
  :bind ("M-TAB" . eldoc-box-help-at-point)
  :config
  (with-eval-after-load 'eglot
    (when (eglot-managed-p)
      (local-set-key (kbd "M-j") (lambda () (interactive) (eldoc-box-scroll-up 3)))
      (local-set-key (kbd "M-k") (lambda () (interactive) (eldoc-box-scroll-down 3))))))

;; :NOTE| Controlling suggestion outputs

(use-package cape
  :defer 1
  :init
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-elisp-symbol)
  (add-hook 'completion-at-point-functions #'cape-tex))

;; :NOTE| Built-in buffer diagnostics

(use-feature flymake
  :commands (flymake-mode)
  :custom
  (flymake-indicator-type 'margins)
  (flymake-margin-indicator-position 'right-margin)
  (flymake-autoresize-margins t)
  (flymake-margin-indicators-string
   '((error   " " compilation-error)
     (warning " " compilation-warning)
     (note    " " compilation-info))))

;; :NOTE| Elegant completion UI

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.05)
  (corfu-auto-prefix 1)
  (corfu-on-exact-match nil))

(use-feature corfu-popupinfo
  :after (corfu)
  :bind (:map corfu-popupinfo-map
              ("M-TAB" . corfu-popupinfo-toggle)
              ("M-k" . corfu-popupinfo-scroll-down)
              ("M-j" . corfu-popupinfo-scroll-up))
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom (corfu-popupinfo-delay nil))

;; :NOTE| Completion through abbreviations

(use-package yasnippet
  :bind ("M-]" . yas-insert-snippet)
  :hook (after-init . yas-global-mode)
  :init (add-to-list 'yas-snippet-dirs
                     (expand-file-name "snippets" irkalla/underworld)))

(use-package yasnippet-snippets :after (yasnippet))

(use-package yasnippet-capf
  :after (cape)
  :config (add-hook 'completion-at-point-functions #'yasnippet-capf)
  :custom (yasnippet-capf-lookup-by 'name))

;; :NOTE| Icons for our completion menu

(use-package kind-icon
  :after (corfu)
  :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  :custom (kind-icon-default-face 'corfu-default))

;; :NOTE| Debug adapter protocol

(use-package dape
  :commands (dape)
  :custom
  (dape-key-prefix "\C-x\C-a")
  (dape-buffer-window-arrangement 'right)
  (dape-cwd-fn 'project-vc-extra-root-markers))

;; :NOTE| A formatter for our messy code

(use-package apheleia
  :commands (apheleia-format-buffer))

;; :NOTE| Language based documentations

(use-package devdocs
  :commands (devdocs-install devdocs-lookup)
  :hook (devdocs-mode . visual-wrap-prefix-mode))

;; :NOTE| Programming problems that remains to be solved

(use-package leetcode
  :commands (leetcode)
  :custom
  (leetcode-save-solutions t)
  (leetcode-prefer-language "python3")
  (leetcode-directory (no-littering-expand-var-file-name "leetcode/")))

(provide 'my-coding)
