;;; lisp/init-melpa.el -*- lexical-binding: t -*-

(require 'package)

(setq package-archives '(("gnu"     . "https://elpa.gnu.org/packages/")
                         ("melpa"   . "https://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Setting defaults for our use-package
(use-package use-package
  :ensure nil
  :custom
  (use-package-verbose t)
  (use-package-expand-minimally t)
  (use-package-always-ensure t)
  ;; (use-package-always-defer t)
  (use-package-compute-statistics t)
  (use-package-minimum-reported-time 0.1)
  (debug-on-error nil))

;; Auto-update packages (every week-end!)
(use-package auto-package-update
  :custom
  (auto-package-update-interval 5)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results nil))

;; Adding ~use-package~ hydra support
(use-package use-package-hydra)

(provide 'init-melpa)
