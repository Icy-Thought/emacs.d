;;; lisp/init-straight.el -*- lexical-binding: t -*-

;; Default straight branch -> develop!
(setq straight-repository-branch "develop")

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(setq use-package-enable-imenu-support t
      use-package-expand-minimally t
      use-package-compute-statistics nil
      use-package-verbose (not (bound-and-true-p byte-compile-current-file)))
(setq byte-compile-warnings '(cl-functions))

(straight-use-package 'use-package)

;; =straight.el= related configurations
(use-package straight
  :custom
  (straight--process-log nil)
  (straight-cache-autoloads t)
  (straight-check-for-modifications '(check-on-save find-when-checking))
  (straight-enable-package-integration nil)
  (straight-vc-git-default-clone-depth 1)
  (straight-use-package-by-default t)
  (use-package-always-ensure nil))

(provide 'init-straight)
