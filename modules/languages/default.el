;;; default.el --- Language Specific Environments -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

;; :NOTE| Time to require the modules

(require 'irkalla/elisp)
(require 'irkalla/cc)
(require 'irkalla/haskell)
(require 'irkalla/lua)
(require 'irkalla/nix)
(require 'irkalla/orgmode)
(require 'irkalla/python)
(require 'irkalla/rust)
(require 'irkalla/typst)
(require 'irkalla/web)
(require 'irkalla/zig)

(provide 'irkalla/languages)
