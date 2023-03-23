;;; lisp/init-shortcuts.el -*- lexical-binding: t -*-

;; Quicker buffer navigation
(global-set-key ["M-["] 'next-buffer)
(global-set-key ["M-]"] 'previous-buffer)

;; Go to scratch buffer
(global-set-key (kbd "<f2>")
                (lambda()
                  (interactive)
                  (switch-to-buffer "*scratch*")))

(provide 'init-shortcuts)
