;;; init-elisp.el --- Langserv: Emacs-Lisp -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Language server configurations for Emacs-Lisp.

;;; Code:

(use-package parinfer-rust-mode
  :hook (emacs-lisp-mode . parinfer-rust-mode)
  :custom
  (parinfer-rust-auto-download t)
  (parinfer-rust-library-directory (no-littering-expand-var-file-name "parinfer-rust/")))


(provide 'init-elisp)
;;; init-elisp.el ends here
