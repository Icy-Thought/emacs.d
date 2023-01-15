;;; early-init.el -*- lexical-binding: t; -*-

;;; Garbage Collector (Faster startup)
(setq gc-cons-threshold (* 50 1000 1000))

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
(setq inhibit-startup-message t)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(mouse-color . "white") default-frame-alist)

;; Avoid flashing white-screen on start
(load-theme 'modus-vivendi t)

;; Load initial buffer faster through fundamental-mode
(customize-set-variable 'initial-major-mode 'fundamental-mode)

