;;; default.el --- Customizing The Emacs User Interface -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

;; :NOTE| Add Irkalla's themes directory to load-paths

(let ((themes-dir (expand-file-name "modules/interface/themes" irkalla/underworld)))
  (when (file-directory-p themes-dir)
    (add-to-list 'load-path themes-dir)
    (add-to-list 'load-path (expand-file-name "template" themes-dir))
    (add-to-list 'custom-theme-load-path themes-dir)))

;; :NOTE| A simplified way to theme Emacs

(use-package autothemer
  :demand t
  :init (load-theme 'kanagawa-wave t))

;; :NOTE| Time to require the modules
(require 'irkalla/icons)
(require 'irkalla/dashboard)
(require 'irkalla/tabline)
(require 'irkalla/modeline)
(require 'irkalla/svg-tags)

(provide 'irkalla/interface)
