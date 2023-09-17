;;; early-init.el --- Icy-Thoughts's Irkalla Emacs Early Initiliazation -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Configurations which ought to be loaded during Irkalla Emacs early initliazation process.

;;; Code:

;; :NOTE| Specifying the location where Irkalla resides
(setq user-emacs-directory "~/.config/emacs")

;; :NOTE| appending UI changes early to Emacs
(setq default-frame-alist
      '((alpha-background     . 85)
        (fullscreen           . nil)
        (menu-bar-lines       . 0)
        (tool-bar-lines       . 0)
        (vertical-scroll-bars . nil))
      initial-frame-alist (copy-alist default-frame-alist))

;; :NOTE| Providing quicker access to Irkalla's directories 
(defvar irkalla/root-dir        (file-truename "~/Workspace/public/emacs.d/irkalla/site-lisp"))
(defvar irkalla/completion-dir  (concat irkalla/root-dir "/completion"))
(defvar irkalla/decorations-dir (concat irkalla/root-dir "/decorations"))
(defvar irkalla/editor-dir      (concat irkalla/root-dir "/editor"))
(defvar irkalla/keymaps-dir     (concat irkalla/root-dir "/keymaps"))
(defvar irkalla/utilities-dir   (concat irkalla/root-dir "/utilities"))

;; :NOTE| adding our directories to the Emacs load-path
(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to our Emacs `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))
(add-subdirs-to-load-path irkalla/root-dir)

(add-to-list 'custom-theme-load-path
             (concat irkalla/decorations-dir "/themes"))

;; Early package modifications
(require 'init-performance)
(require 'init-packages)
(require 'init-options)

;; :NOTE| Changing the behaviour of custom.el
(setq-default custom-file
              (expand-file-name "etc/custom.el" user-emacs-directory))

(if (file-exists-p custom-file)
    (load custom-file 'noerror 'nomessage)
  (with-temp-buffer (write-file custom-file)))

;; Prevent certain buffers from being killed
(with-current-buffer "*scratch*"  (emacs-lock-mode 'kill))
(with-current-buffer "*Messages*" (emacs-lock-mode 'kill))

;;; early-init.el ends here
