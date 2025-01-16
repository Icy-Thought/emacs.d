;;; init.el --- Laboratory of Irkalla -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

;; :NOTE| Bootstrap Elpaca, the package manager

(require 'my-elpaca)

;; :NOTE| Require the remaining modules

(require 'my-lib)
(require 'my-core)
(require 'my-interface)
(require 'my-editor)
(require 'my-languages)
(require 'my-utilities)

(provide 'init)
