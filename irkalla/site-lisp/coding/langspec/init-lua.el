;;; init-lua.el --- Language Enviornment for Lua -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; A minor language environment config for Lua.

;;; Code:

(use-package lua-mode
  :mode ("\\.lua\\'" . lua-mode)
  :hook (lua-mode . eglot-ensure)
  :config
  (when (executable-find "lua-language-server")
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs `(lua-mode . ("lua-language-server"))))))

;; :NOTE| apheleia formatting support
(when (executable-find "stylua")
  (with-eval-after-load 'apheleia
    (setf (alist-get 'stylua apheleia-formatters)
          '("stylua" "--config-path" (expand-file-name "~/.config/stylua/stylua.toml") "-"))
    (add-to-list 'apheleia-mode-alist '(lua-mode . stylua))))

(provide 'init-lua)
;;; init-lua.el ends here
