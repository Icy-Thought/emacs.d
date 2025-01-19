;;; my-nova.el --- Visual Enhancements Applied To Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-package nova
  :disabled t ;; broken!
  :vc (:url "https://github.com/thisisran/nova")
  :hook ((corfu-mode . nova-corfu-mode)
         (corfu-popupinfo-mode . nova-corfu-popupinfo-mode)
         (vertico-mode . nova-vertico-mode)
         (eldoc-mode . nova-eldoc-mode)))

(provide 'my-nova)
