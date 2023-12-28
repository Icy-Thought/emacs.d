;;; init-editor.el --- Editor-related Configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; A place for us to retain short and built-in editor-related Emacs customizations.

;;; Code:

(use-package emacs
  :elpaca nil
  :hook (text-mode . visual-line-mode)
  :custom 
  (confirm-nonexistent-file-or-buffer nil)
  (backward-delete-char-untabify-method 'hungry)
  (word-wrap nil)
  (fill-column 120)
  (truncate-lines t)
  (truncate-string-ellipsis "↴")
  (electric-indent-inhibit t)
  (indent-tabs-mode nil)
  (standard-indent 4)
  (tab-width 4)
  (find-file-suppress-same-file-warnings t)
  (remote-file-name-inhibit-locks t)
  (x-stretch-cursor t))

(use-package auto-revert
  :elpaca nil
  :hook ((prog-mode text-mode) . auto-revert-mode)
  :custom
  (auto-revert-interval 1)
  (auto-revert-notify t)
  (auto-revert-verbose t))

(use-package editorconfig
  :hook (prog-mode . editorconfig-mode))

(use-package display-line-numbers
  :elpaca nil
  :hook ((prog-mode text-mode conf-mode) . display-line-numbers-mode)
  :custom (display-line-numbers-type 'relative))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package elec-pair
  :elpaca nil
  :hook ((prog-mode text-mode) . (lambda ()
                                   (if (not (derived-mode-p 'emacs-lisp-mode 'lisp-mode))
                                       (electric-pair-local-mode))))
  :custom (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(use-package hl-line
  :elpaca nil
  :hook ((prog-mode text-mode) . hl-line-mode))

(use-package frames
  :elpaca nil
  :hook ((prog-mode text-mode) . window-divider-mode)
  :custom
  (window-divider-default-places t)
  (window-divider-default-right-width 2)
  (window-divider-default-bottom-width 2))

(use-package olivetti
  :commands (olivetti-mode)
  :custom (olivetti-body-width 120)
  :config
  (defun irkalla/zen-mode ()
    "Toggle buffer appearance for a touch of sophistication."
    (interactive)
    (cond
     (buffer-face-mode
      (display-line-numbers-mode +1)
      (olivetti-mode -1)
      (text-scale-increase 0.0)
      (buffer-face-mode -1))
     (t (display-line-numbers-mode -1)
        (olivetti-mode +1)
        (olivetti-set-width 80)
        (text-scale-increase 1.5)
        (setq-local buffer-face-mode-face '(:family "Dancing Script"))
        (buffer-face-mode +1)))))

;; :NOTE| now we import our custom modules
(irkalla/enable-modules
 (evil ligatures history ediff region citar whitespace))

;; :NOTE| Lastly, setup hydra's for our ever-growing bindings
(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define+ main-hydra ()
    ("Editor"
     (("e" evil-hydra/body   "Evil")
      ("C" olivetti-mode     "Center Text")
      ("z" irkalla/zen-mode  "Zen-Mode"))))

  (pretty-hydra-define+ visual-main-hydra ()
    ("Editor"
     (("r" region-hydra/body "Region")))))

(provide 'init-editor)
;;; init-editor.el ends here
