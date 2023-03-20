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

;; Reduce noise on cold-start.
(setq inhibit-startup-message t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t)

;; Get rid of "For information about GNU Emacs..." message on startup.
(advice-add #'display-startup-echo-area-message :override #'ignore)
(advice-add #'display-startup-screen :override #'ignore)

;; Quicker startup when in `fundamental-mode`!
(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(unless init-file-debug
  ;; Get rid of 'loading X file' echo..
  (define-advice load-file (:override (file) silence)
    (load file nil 'nomessage))
  ;; Undo advice until later for proper loading..
  (define-advice startup--load-user-init-file (:before (&rest _) undo-silence)
    (advice-remove #'load-file #'load-file@silence))

  ;; Disable default Emacs mode-line for a minor launch time boost. (30 - 50ms)
  (put 'mode-line-format 'initial-value (default-toplevel-value 'mode-line-format))
  (setq-default mode-line-format nil)
  (dolist (buf (buffer-list))
    (with-current-buffer buf (setq mode-line-format nil))))

;;; UI configuration
;; Remove bloated UI elements
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Set transparency early
(add-to-list 'default-frame-alist '(alpha-background . 85))
(set-frame-parameter (selected-frame) 'alpha-background 85)

;; Add padding to the displayed text
(fringe-mode 17)
