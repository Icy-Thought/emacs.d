;;; site-start.el --- Icy-Thoughts's Irkalla Emacs Early Initiliazation -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Configurations which ought to be loaded during Irkalla Emacs initliazation process.

;;; Code:

;; :NOTE| Defining our custom directories
(defvar irkalla/root-dir       (file-truename "~/.config/emacs/site-lisp"))
(defvar irkalla/core-dir       (concat irkalla/root-dir "/core"))
(defvar irkalla/aesthetics-dir (concat irkalla/root-dir "/aesthetics"))
(defvar irkalla/editor-dir     (concat irkalla/root-dir "/editor"))
(defvar irkalla/keymaps-dir    (concat irkalla/root-dir "/keymaps"))
(defvar irkalla/utilities-dir  (concat irkalla/root-dir "/utilities"))
(defvar irkalla/completion-dir (concat irkalla/root-dir "/completion"))

(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to our Emacs `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))
(add-subdirs-to-load-path "~/.config/emacs/site-lisp/")

(require 'init)
