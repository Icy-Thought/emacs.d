;;; custom-modes.el --- Custom Emacs Modes -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(defun irkalla/manuscript-toggle ()
  "Toggle buffer appearance for a touch of sophistication."
  (if (eq (symbol-value 'fontaine-current-preset) 'regular)
      (fontaine-set-preset 'reading)
    (fontaine-set-preset 'regular)))

(define-minor-mode irkalla/manuscript-mode
  "Paint our buffers with the ancient manuscript style."
  :group 'irkalla
  :global nil
  (irkalla/manuscript-toggle))

(provide 'irkalla/reading)
