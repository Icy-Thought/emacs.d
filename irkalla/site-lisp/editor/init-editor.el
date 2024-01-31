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
  :custom 
  (confirm-nonexistent-file-or-buffer nil)
  (backward-delete-char-untabify-method 'hungry)
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

(use-package visual-fill-column
  :elpaca nil
  :hook ((visual-line-mode . visual-fill-column-mode)
         (text-mode . visual-line-mode)) 
  :custom (visual-fill-column-center-text t)
  :config
  (defun irkalla/zen-mode ()
    "Toggle buffer appearance for a touch of sophistication."
    (interactive)
    (if (bound-and-true-p buffer-face-mode)
        (progn
          (visual-line-mode -1)
          (visual-fill-column-mode -1)
          (text-scale-increase 0.0)
          (buffer-face-mode -1)
          (setq-local fill-column 120))
      (visual-line-mode +1)
      (setq-local fill-column 80
                  buffer-face-mode-face '(:family "Dancing Script"))
      (buffer-face-mode +1)
      (text-scale-increase 1.5))))

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
                                   (when (not (derived-mode-p 'emacs-lisp-mode 'lisp-mode))
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

;; :NOTE| Setup hydra's for the ever-growing bindings
(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define editor-hydra
    (:title (pretty-hydra-title "──｢ Chrysaora Melanaster ｣──" 'mdicon "nf-md-graph_outline")
            :color teal :quit-key "q")
    ("Action"
     (("z" irkalla/zen-mode    "Zen-Mode" :toggle t)
      ("b" eval-buffer         "Eval Buf."))))

  (pretty-hydra-define visual-editor-hydra
    (:title (pretty-hydra-title "──｢ (Visual) Chrysaora Melanaster ｣──" 'mdicon "nf-md-graph_outline")
            :color teal :quit-key "q")
    ("Action"
     (("e" eval-region "Eval Region"))))

  (with-eval-after-load 'evil
    (evil-global-set-key 'normal (kbd ",") 'editor-hydra/body)
    (evil-global-set-key 'visual (kbd ",") 'visual-editor-hydra/body))

  (with-eval-after-load 'meow
    (meow-normal-define-key '("," . editor-hydra/body))))

;; :NOTE| Import the custom modules
(irkalla/enable-modules
 (evil ligatures history ediff magit region citar whitespace coding))

(provide 'init-editor)
;;; init-editor.el ends here
