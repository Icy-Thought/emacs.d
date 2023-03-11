;;; early-init.el -*- lexical-binding: t; -*-

;; Init `use-package` at a later stage
(setq package-enable-at-startup nil)

;;; Garbage Collector (faster startup)
(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 250 1000 1000)
                      gc-cons-percentage 0.0001)))

;; Don’t compact font caches during GC
(setq inhibit-compacting-font-caches t)

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

  (add-to-list 'native-comp-eln-load-path
               (expand-file-name "eln-cache/" user-emacs-directory)))

;;; UI configuration
;; Remove bloated UI elements
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Set transparency early
(add-to-list 'default-frame-alist '(alpha-background . 85))
(set-frame-parameter (selected-frame) 'alpha-background 85)

;; Add frame borders and window dividers (TODO: apply alpha)
;; (add-to-list 'default-frame-alist '(internal-border-width . 25))

;; Avoid flashing white-screen on start
(load-theme 'modus-vivendi t)
