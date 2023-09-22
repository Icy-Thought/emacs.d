;;; init-olivetti.el --- Olivetti: Center Displayed Text -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Centering text on demand when reading has proven to be rather useful for quicker eye skimming, hence why we are
;; adding its support.

;;; Code:

(use-package olivetti
  :defer t
  :preface
  (defun irkalla/no-distractions ()
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
         (buffer-face-mode +1))))
  :general
  (irkalla/comma-lead-keydef
    "q o" '(olivetti-mode           :which-key "Center Buffer Text!")
    "b f" '(irkalla/no-distractions :which-key "Distraction-free reading/writing"))
  :custom (olivetti-body-width 120))

(provide 'init-olivetti)
;;; init-olivetti.el ends here
