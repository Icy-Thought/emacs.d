;;; site-start.el --- Icy-Thoughts's Irkalla Emacs Early Initiliazation -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Configurations which ought to be loaded during Irkalla Emacs initliazation process.

;;; Code:

(defcustom irkalla/default-font-family "VictorMono Nerd Font"
  "The default font of our Irkalla Emacs."
  :type 'string
  :group 'irkalla)

(setq default-frame-alist
      (append (list
               '(alpha-background . 85)
               `(font . ,(concat irkalla/default-font-family "-13.5:weight=bold:antialias=true"))
               '(height . 125)
               '(mouse-color . "white")

               ;; :NOTE| Disabling unnecessary bloat..
               '(fullscreen . nil)
               '(menu-bar-lines . 0)
               '(tool-bar-lines . 0)
               '(vertical-scroll-bars . nil))))

(set-fontset-font "fontset-default"
                  'arabic (font-spec :family "Scheherazade New;" :size 25)
                  (charsetp 'chinese-gb18030) (font-spec :family "Sarasa Gothic SC" :size 25))

;; :NOTE| Defining our custom directories
(setq user-emacs-directory "~/.config/emacs")

(defvar irkalla/root-dir       (file-truename "~/Workspace/public/emacs.d/irkalla/site-lisp"))
(defvar irkalla/core-dir       (concat irkalla/root-dir "/core"))
(defvar irkalla/aesthetics-dir (concat irkalla/root-dir "/aesthetics"))
(defvar irkalla/editor-dir     (concat irkalla/root-dir "/editor"))
(defvar irkalla/keymaps-dir    (concat irkalla/root-dir "/keymaps"))
(defvar irkalla/utilities-dir  (concat irkalla/root-dir "/utilities"))
(defvar irkalla/completion-dir (concat irkalla/root-dir "/completion"))

;; :NOTE| adding our directories to the Emacs load-path
(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to our Emacs `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))
(add-subdirs-to-load-path irkalla/root-dir)

(add-to-list 'custom-theme-load-path
             (concat irkalla/aesthetics-dir "/themes"))

(require 'init)
