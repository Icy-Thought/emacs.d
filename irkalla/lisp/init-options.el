;;; lisp/init-options.el -*- lexical-binding: t -*-

;; Identification for GPG-signature and whatnot...
(setq user-full-name "Icy-Thought"
      user-mail-address "icy-thought@pm.me")

(use-package emacs
  :elpaca nil
  :init
  (global-auto-revert-mode t)
  (global-hl-line-mode t)
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  (global-subword-mode t)
  (save-place-mode t)
  :custom
  (auto-save-interval 50)
  (confirm-nonexistent-file-or-buffer nil)
  (fill-column 120)
  (echo-keystrokes 0.02)
  (enable-local-variables t)
  (enable-recursive-minibuffers t)
  (find-file-suppress-same-file-warnings t)
  (help-window-select t)
  (inhibit-startup-echo-area-message t)
  (kill-whole-line t)
  (load-prefer-newer t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (recenter-positions '(top middle bottom))
  (remote-file-name-inhibit-locks t)
  (require-final-newline t)
  (ring-bell-function 'ignore)
  (search-whitespace-regexp nil)
  (sentence-end-double-space nil)
  (use-dialog-box nil)
  (use-short-answers t)
  (vc-follow-symlinks t)
  (vc-make-backup-files t)
  (version-control t))

;; Set time-stamp to 24-hour format
(use-package time
  :elpaca nil
  :custom
  (display-time-24hr-format t)
  (display-time-day-and-date t))

;; Whitespace + Cleanup
(use-package whitespace
  :elpaca nil
  ;; :hook (prog-mode . whitespace-mode)
  :custom
  (whitespace-action '(cleanup auto-cleanup))
  (whitespace-style
   '(face spaces tabs newline trailing space-mark tab-mark newline-mark))
  (whitespace-display-mappings
   '(;; space -> · else .
     (space-mark 32 [183] [46])
     ;; new line -> ¬ else $
     (newline-mark ?\n [172 ?\n] [36 ?\n])
     ;; carriage return (Windows) -> ¶ else #
     (newline-mark ?\r [182] [35])
     ;; tabs -> » else >
     (tab-mark ?\t [187 ?\t] [62 ?\t]))))

;; Relative number line
(use-package display-line-numbers
  :elpaca nil
  :hook ((text-mode prog-mode conf-mode) . display-line-numbers-mode)
  :custom (display-line-numbers-type 'relative))

;; Inserts matching pair (built-in surround!!)
(use-package elec-pair
  :elpaca nil
  :hook ((org-mode
          markdown-mode
          prog-mode) . electric-pair-mode)
  :config (add-hook 'emacs-lisp-mode-hook (lambda () (electric-pair-mode 0))))

(provide 'init-options)
