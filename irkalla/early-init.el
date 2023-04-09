;;; early-init.el -*- lexical-binding: t; -*-

;; Init `use-package` at a later stage
(setq package-quickstart nil
      package-enable-at-startup nil)

;; Don’t compact font caches during GC
(setq inhibit-compacting-font-caches t)

;; DON'T resize frame on cold start
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;;; Prefer loading recently compiled .el files
(customize-set-variable 'load-prefer-newer t)

;; Reduce noise on cold-start.
(setq inhibit-startup-buffer-menu t
      inhibit-startup-message t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)

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

;; Increasing maximum processing ability
(when (boundp 'read-process-output-max)
  (setq read-process-output-max (* 24 1024 1024)))

;; Native-Comp warnings..
(customize-set-variable 'native-comp-async-report-warnings-errors nil)
(customize-set-variable 'native-comp-speed 2)
(customize-set-variable 'native-comp-deferred-compilation t)

;; Settings default encoding early
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

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

(provide 'early-init)
