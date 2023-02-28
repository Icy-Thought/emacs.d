;;; early-init.el -*- lexical-binding: t; -*-

;; Init `use-package` at a later stage
(setq package-enable-at-startup nil)

;;; Garbage Collector (faster startup)
(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
	  (lambda () (setq gc-cons-threshold (* 2 1000 1000))))

;; DON'T resize frame on cold start
(setq frame-inhibit-implied-resize t)

;;; Prefer loading recently compiled .el files
(customize-set-variable 'load-prefer-newer t)

;;; Native-Comp
(when (featurep 'native-compile)
  (setq native-comp-deferred-compilation t)
  (setq native-comp-async-report-warnings-errors nil)

  ;; Set correct dir for native compilation cache storage
  (when (fboundp 'startup-redirect-eln-cache)
    (if (version< emacs-version "29")
        (add-to-list 'native-comp-eln-load-path (convert-standard-filename (expand-file-name "var/eln-cache/" user-emacs-directory)))
      (startup-redirect-eln-cache (convert-standard-filename (expand-file-name "var/eln-cache/" user-emacs-directory)))))

  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

;;; UI configuration
;; Remove bloated UI elements
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Avoid flashing white-screen on start
(load-theme 'modus-vivendi t)

;; Load initial buffer faster through fundamental-mode
(customize-set-variable 'initial-major-mode 'fundamental-mode)
