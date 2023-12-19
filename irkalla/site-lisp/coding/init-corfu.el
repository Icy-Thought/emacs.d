;;; init-corfu.el --- Corfu: COmpletion in Region FUnction  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; A completion menu with the ability to provide fancy completions for the Emacs user.

;;; Code:

(use-package corfu
  :elpaca (:files (:defaults "extensions/*.el"))
  :requires (kind-icons)
  :preface
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (setq-local corfu-echo-delay nil
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  :hook ((elpaca-after-init . global-corfu-mode)
         (minibuffer-setup . corfu-enable-in-minibuffer))
  :general (:states 'insert :keymaps 'corfu-map
                    "TAB"   #'corfu-next
                    [tab]   #'corfu-next
                    "S-TAB" #'corfu-previous
                    [backtab] #'corfu-previous)
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-preselect t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.05)
  (corfu-count 16)
  (corfu-scroll-margin 5)
  (corfu-separator ?\s)
  (corfu-on-exact-match nil)
  (corfu-preview-current 'insert)
  (corfu-quit-no-match 'separator)
  :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package corfu-terminal
  :unless window-system
  :hook (corfu-mode . corfu-terminal-mode))

;; :NOTE| Posframe like completion menu
(use-package corfu-popupinfo
  :elpaca nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom (corfu-popupinfo-delay '(0.5 . 0.2)))

;; :NOTE| Providing corfu with icons for better completion menu
(use-package kind-icon
  :requires (svg-lib)
  :custom
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08))

;; :NOTE| Completion at point through Cape
(use-package cape
  :preface
  (defun setup-cape-extensions ()
    (dolist (cape-fn '(file dabbrev elisp-symbol tex keyword emoji))
      (add-to-list 'completion-at-point-functions (intern (format "cape-%s" cape-fn)))))
  :hook (corfu-mode . setup-cape-extensions))

(provide 'init-corfu)
;;; init-corfu.el ends here
