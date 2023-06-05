;;; editor/init-editor.el -*- lexical-binding: t -*-

;; Require custom editor modules:
(require 'init-evil)
;; (require 'init-meow)
;; (require 'init-indent) ;; :FIX| broken..
(require 'init-orgmode)
(require 'init-bib)

;; :NOTE| Built-in configurations
(use-package emacs
  :elpaca nil
  :config
  ;; :WARN| Smooth scrolling (Emacs >= 29)
  (when (boundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode 1))

  ;; :TODO| Auto-break longer lines
  (dolist (mode '(org-mode markdown-mode text-mode))
    (add-hook (intern (concat (symbol-name mode) "-hook"))
              (lambda ()
                (visual-line-mode 1)
                (auto-fill-mode 1))))
  :custom
  (electric-indent-inhibit t)
  (indent-tabs-mode nil)
  (standard-indent 4)
  (tab-width 4)
  (undo-limit 6710886400) ;; 64mb
  (undo-strong-limit 100663296) ;; x 1.5 (96mb)
  (undo-outer-limit 1006632960) ;; x 10 (960mb), (Emacs uses x100), but this seems too high.
  (word-wrap nil))

;; Manage our Emacs history properly
(use-package savehist
  :elpaca nil
  :init (savehist-mode 1)
  :custom
  (history-length 1000)
  (history-delete-duplicates t)
  (savehist-autosave-interval 60)
  (savehist-save-minibuffer-history t)
  (savehist-file (expand-file-name "savehist" user-emacs-cache-directory)))

;; Navigate to last known location of buffer
(use-package saveplace
  :elpaca nil
  :init (save-place-mode t)
  :custom
  (save-place-file (expand-file-name "saveplace" user-emacs-cache-directory))
  (save-place-forget-unreadable-files t))

;;; :NOTE| External Packages

(use-package editorconfig
  :defer 1
  :custom (editorconfig-mode 1))

;; Auto-format code!
(use-package format-all
  :config
  :hook ((prog-mode . format-all-mode)
         (prog-mode . format-all-ensure-formatter))
  :config
  (setq-default format-all-formatters
                '(("Nix" alejandra)
                  ("Haskell" stylish-haskell)
                  ("Rust" rustfmt))))

;; Code linting
(use-package flycheck
  :defer t
  :hook (prog-mode . global-flycheck-mode)
  :custom (flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; Spelling
(use-package jinx
  :elpaca nil
  :hook (elpaca-after-init . global-jinx-mode)
  :general (general-nmap "z =" '(jinx-correct :which-key "Correct the damned misspellings...")))

;; Center content + display minimap for current buffer
(use-package olivetti
  :hook (elpaca-after-init . olivetti-mode)
  :general (irkalla/comma-lead-keydef
             "q" '(olivetti-mode :which-key "Distraction free writing!"))
  :custom
  (olivetti-body-width 0.7)
  (olivetti-minimum-body-width 115)
  (olivetti-recall-visual-line-mode-entry-state t))

;; Fold code like paper
(use-package ts-fold
  :elpaca (ts-fold :host github :repo "emacs-tree-sitter/ts-fold")
  :init (global-ts-fold-mode))

;; Perspective.el <-
(use-package perspective
  :init (persp-mode)
  :general (general-nmap "C-x C-b" '(persp-list-buffers :which-key "C-x C-b, but with perspective-buf filter"))
  :custom (persp-mode-prefix-key (kbd "C-c M-p")))

;; Colorize hex color names in buffer
(use-package rainbow-mode
  :defer 1
  :hook (prog-mode . rainbow-mode))

;; Tree-based undo system
(use-package undo-tree
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t))

(provide 'init-editor)
