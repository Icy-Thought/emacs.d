;;; init-eldoc.el --- Eldoc: $SYMB Documentation -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Provide information about the $SYMB at point in a nice UI posframe.

;;; Code:

(use-package eldoc
  :ensure nil
  :hook (eglot-managed-mode . eldoc-mode)
  :custom
  (eldoc-idle-delay 1.0)
  (eldoc-echo-area-display-truncation-message nil)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly))

(use-package eldoc-box
  :after (eldoc)
  :commands (eldoc-box-help-at-point)
  :hook (eldoc-mode . eldoc-box-hover-at-point-mode)) 

(provide 'init-eldoc)
;;; init-eldoc.el ends here
