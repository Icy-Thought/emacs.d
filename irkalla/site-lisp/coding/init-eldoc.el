;;; init-eldoc.el --- Eldoc: $SYMB Documentation -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Provide information about the $SYMB at point in a nice UI posframe.

;;; Code:

(use-package eldoc
  :elpaca nil
  :custom
  (eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  (eldoc-echo-area-display-truncation-message nil)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-idle-delay 1.0))

(use-package eldoc-box
  :after eldoc
  :general (:states '(normal operator) :keymaps 'prog-mode-map
             "TAB" '(eldoc-box-help-at-point :which-key "LSP info at point")))

(provide 'init-eldoc)
;;; init-eldoc.el ends here
