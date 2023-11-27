;;; init-direnv.el --- Direnv: Local Environment Variables -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Apply dir-local environment variables for quick-access to dir specific nixpkgs

;;; Code:

(use-package direnv
  :hook (elpaca-after-init . direnv-mode)
  :config (add-to-list 'warning-suppress-types '(direnv))
  :custom (direnv-always-show-summary nil))

(provide 'init-direnv)
;;; init-direnv.el ends here
