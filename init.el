;;; init.el --- Laboratory of Irkalla -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

;; :NOTE| Bootstrap Elpaca, the package manager

(require 'irkalla/elpaca)

;; :NOTE| Require the remaining modules

(require 'irkalla/lib)
(require 'irkalla/core)
(require 'irkalla/interface)
(require 'irkalla/editor)
(require 'irkalla/lanuages)
(require 'irkalla/utilities)

(provide 'init)
