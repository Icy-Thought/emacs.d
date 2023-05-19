;;; editor/init-editor.el -*- lexical-binding: t -*-

;; Require custom editor modules:
(require 'init-evil)
;; (require 'init-meow)
;; (require 'init-indent) ;; :FIXME| broken..
(require 'init-orgmode)

;; Indentation: 2 -> 4 + tabs -> spaces
(use-package emacs
  :elpaca nil
  :init
  ;; WARN: Smooth scrolling (Emacs >= 29)
  (when (boundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode 1))
  :custom
  (electric-indent-inhibit t)
  (indent-tabs-mode nil)
  (standard-indent 4)
  (tab-width 4)
  (undo-limit 6710886400) ;; 64mb
  (undo-strong-limit 100663296) ;; x 1.5 (96mb)
  (undo-outer-limit 1006632960) ;; x 10 (960mb), (Emacs uses x100), but this seems too high.
  (word-wrap nil))

;; Built-in configurations
(use-package savehist
  :elpaca nil
  :init (savehist-mode 1)
  :custom
  (history-length 1000)
  (history-delete-duplicates t)
  (savehist-autosave-interval 60)
  (savehist-save-minibuffer-history t)
  (savehist-file (expand-file-name "savehist" user-emacs-cache-directory)))

(use-package saveplace
  :elpaca nil
  :init (save-place-mode t)
  :custom
  (save-place-file (expand-file-name "saveplace" user-emacs-cache-directory))
  (save-place-forget-unreadable-files t))

;;; External Packages

;; Auto-format code!
(use-package format-all
  :config
  :hook ((prog-mode . format-all-mode)
         (prog-mode . format-all-ensure-formatter))
  :config
  (setq-default format-all-formatters
                '(("Nix" nixfmt)
                  ("Haskell" stylish-haskell)
                  ("Rust" rustfmt))))

;; Code linting
(use-package flycheck
  :hook (prog-mode . global-flycheck-mode)
  :custom (flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; Spelling
(use-package jinx
  :elpaca nil
  :hook (emacs-startup . global-jinx-mode)
  :bind ([remap ispell-word] . jinx-correct))

;; Fold code like paper
(use-package ts-fold
  :elpaca (ts-fold :host github :repo "emacs-tree-sitter/ts-fold")
  :init (global-ts-fold-mode))

;; Perspective.el <-
(use-package perspective
  :bind ("C-x C-b" . persp-list-buffers)
  :custom (persp-mode-prefix-key (kbd "C-c M-p"))
  :init (persp-mode))

;; Colorize hex color names in buffer
(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

;; Tree-based undo system
(use-package undo-tree
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t))

(provide 'init-editor)
