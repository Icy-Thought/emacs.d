;;; init.el -*- lexical-binding: t; -*-

;; GC pauses -> faster (decrease threshold)
(setq gc-cons-threshold (* 2 1000 1000))

;; Initialize --debug-init on error
(setq debug-on-error init-file-debug)

;;; Load `lisp` & `plugins` direcotries into path
(add-to-list 'load-path "~/.config/emacs/lisp")

(add-to-list 'load-path "~/.config/emacs/plugins/completion")
(add-to-list 'load-path "~/.config/emacs/plugins/editor")
(add-to-list 'load-path "~/.config/emacs/plugins/langserv")
(add-to-list 'load-path "~/.config/emacs/plugins/toolset")
(add-to-list 'load-path "~/.config/emacs/plugins/ui")

;;; Use-Packages: ease of package management
(require 'init-melpa)

;; Essentials: structured emacs mods
(require 'init-management)
(require 'init-decorations)
(require 'init-options)
(require 'init-shortcuts)

;; Plugin: categorized modules
(require 'init-completion)
(require 'init-langserv)
(require 'init-editor)
(require 'init-toolset)
(require 'init-ui)
