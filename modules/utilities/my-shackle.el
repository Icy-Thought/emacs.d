;;; my-shackle.el --- Manage Placement Of Buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-package shackle
  :hook (elpaca-after-init . shackle-mode)
  :custom
  (shackle-default-size 0.33)
  (shackle-rules
   `((help-mode                            :align right :select t :size 0.45)
     (helpful-mode                         :align right :select t :size 0.45)
     (compilation-mode                     :align right)
     (flymake-diagnostics-buffer-mode      :align below)
     (magit-process-mode                   :align below)
     ("*eldoc*"                            :align right)
     ("*Messages*"                         :align below)
     ("*Async-native-compile-log*"         :align right)
     ("*devdocs*"                          :align right :select t :same t :inhibit-window-quit t)
     ("*mu4e-headers*"                     :align right :select t :size 0.75)
     ;; also launch without invoking J -> inbox manual select -> head into inbox by defeault
     (,(rx "*" (* any) "REPL" (* any) "*") :align right :regexp t)
     (,(rx bos "*" (* any)
           (| "eat" "eshell" "shell" "term" "vterm")
           (* any) "*" eos)
      :align right :select t :regexp t :size 0.75))))

(provide 'my-shackle)
