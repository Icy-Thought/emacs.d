;;; default.el --- Empowering The Editing Environment -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-feature emacs
  :config
  (setopt backward-delete-char-untabify-method 'hungry
          confirm-nonexistent-file-or-buffer nil
          cursor-in-non-selected-windows nil
          electric-indent-inhibit t
          indent-tabs-mode nil
          tab-width 4
          remote-file-name-inhibit-locks t
          shell-kill-buffer-on-exit t
          text-mode-ispell-word-completion nil
          x-stretch-cursor t
          x-underline-at-descent-line t))

;; :NOTE| Display line number

(use-feature display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  :custom (display-line-numbers-type 'relative))

;; :NOTE| Color #HEX & parantheses & indentation levels

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package indent-bars
  :ensure (:host github :repo "jdtsmith/indent-bars")
  :hook (prog-mode . indent-bars-mode)
  :custom
  (indent-bars-zigzag nil)
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module")))

;; :NOTE| Highlight active line

(use-feature hl-line
  :hook (elpaca-after-init . global-hl-line-mode)
  :config
  (setopt hl-line-sticky-flag nil
          hl-line-range-function
          (lambda () (cons (line-beginning-position) (line-end-position)))))

;; :NOTE| Insert matching $SYMB

(use-feature elec-pair
  :hook ((prog-mode text-mode) . electric-pair-local-mode)
  :custom (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; :NOTE| Highlight matching parantheses

(use-feature paren
  :hook (prog-mode . show-paren-mode)
  :custom
  (show-paren-delay 0)
  (show-paren-style 'parenthesis)
  (show-paren-when-point-in-periphery nil)
  (show-paren-when-point-inside-paren nil))

;; :NOTE| Sub-word based navigation

(use-feature subword
  :hook ((prog-mode text-mode) . subword-mode))

;; :NOTE| Regional based selection

(use-package expand-region
  :commands (er/expand-region er/contract-region))

;; :NOTE| Regexp based alignment

(use-package ialign :commands (ialign))

;; :NOTE| Elegant spell-checker

(use-feature jinx
  :commands (jinx-correct)
  :hook (text-mode . jinx-mode)
  :bind ([remap ispell-word] . jinx-correct))

;; :NOTE| location indicator for larger projects

(use-package breadcrumb
  :hook (prog-mode . breadcrumb-local-mode)
  :config (fset 'breadcrumb--project-crumbs-1 #'ignore)
  :custom (breadcrumb-project-max-length -1))

;; :NOTE| Time to require the modules

(require 'irkalla/evil)
(require 'irkalla/frames)
(require 'irkalla/files)
(require 'irkalla/history)
(require 'irkalla/content)
(require 'irkalla/treesitter)
(require 'irkalla/development)

(provide 'irkalla/editor)
