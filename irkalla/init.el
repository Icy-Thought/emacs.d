;;; init.el -*- lexical-binding: t; -*-

;; GC pauses -> faster (decrease threshold)
(setq gc-cons-threshold (* 2 1000 1000))

;; Initialize --debug-init on error
(setq debug-on-error init-file-debug)

;;; Load `lisp` & `plugins` direcotries into path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(add-to-list 'load-path (expand-file-name "plugins/completion" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "plugins/editor" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "plugins/langserv" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "plugins/toolset" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "plugins/ui" user-emacs-directory))

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

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
