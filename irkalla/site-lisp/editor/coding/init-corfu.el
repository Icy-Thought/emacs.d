;;; init-corfu.el --- Corfu: COmpletion in Region FUnction  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; A completion menu with the ability to provide fancy completions for the Emacs user.

;;; Code:

(use-package corfu
  :ensure (:files (:defaults "extensions/*.el"))
  :after (kind-icon)
  :preface
  (defun corfu-always-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input)
                (eq (current-local-map) read-passwd-map))
      (setq-local corfu-echo-delay nil
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  :hook ((elpaca-after-init . global-corfu-mode)
         (minibuffer-setup . corfu-always-enable-in-minibuffer))
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous))
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
  :config
  (when (featurep 'kind-icon)
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

(use-package corfu-terminal
  :unless window-system
  :after (corfu)
  :hook (corfu-mode . corfu-terminal-mode))

;; :NOTE| Posframe like completion menu
(use-feature corfu-popupinfo
  :after (corfu)
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom (corfu-popupinfo-delay '(0.5 . 0.2)))

;; :NOTE| Providing corfu with icons for better completion menu
(use-package kind-icon
  :demand t
  :custom
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08))

;; :NOTE| Completion at point through Cape
(use-package cape
  :demand t
  :custom (cape-dict-file (getenv "WORDLIST"))
  :config
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions #'cape-file nil t)
              (add-hook 'completion-at-point-functions #'cape-keyword nil t)))
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (add-hook 'completion-at-point-functions #'cape-elisp-symbol nil t)))
  (add-hook 'LaTeX-mode-hook
            (lambda () (add-hook 'completion-at-point-functions #'cape-tex nil t)))
  (add-hook 'text-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions #'cape-elisp-block nil t)
              (add-hook 'completion-at-point-functions #'cape-dict nil t)
              (add-hook 'completion-at-point-functions #'cape-emoji nil t))))

(provide 'init-corfu)
;;; init-corfu.el ends here
