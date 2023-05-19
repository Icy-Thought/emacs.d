;;; lisp/init-packages.el -*- lexical-binding: t -*-

(require 'package)

(setq package-archives '(("gnu"     . "https://elpa.gnu.org/packages/")
                         ("melpa"   . "https://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package use-package
  :ensure nil
  :custom
  (use-package-verbose t)
  (use-package-always-ensure t)
  ;; (use-package-always-defer t)
  (use-package-expand-minimally t)
  (use-package-compute-statistics t)
  (use-package-minimum-reported-time 0.1)
  (debug-on-error nil))

;; Autoload Nix installed packages (built-in) properly!
(dolist (path load-path)
  (when (string-match-p "/nix/store/[a-z0-9]\\{32\\}-emacs-packages-deps.*" path)
    (dolist (autoload-file (directory-files path t "-autoloads.el"))
      (with-demoted-errors "init.el error: %s"
        (load autoload-file nil t)))))

(provide 'init-packages)
