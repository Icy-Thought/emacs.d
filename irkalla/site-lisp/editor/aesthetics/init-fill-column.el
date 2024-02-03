;;; init-fill-column.el --- Fill Column: chunk-sized lines  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Line length should not be stretched to the end of the screen, rather it should be broken down into smaller chunks!

;;; Code:

(use-package emacs
  :elpaca nil
  :hook (text-mode . visual-line-mode)
  :custom
  (fill-column 120)
  (truncate-lines t)
  (truncate-string-ellipsis "↴"))

(use-package visual-fill-column
  :elpaca nil
  :commands (visual-fill-column-mode)
  :preface
  (defun irkalla/manuscript-toggle ()
    "Toggle buffer appearance for a touch of sophistication."
    (if (bound-and-true-p buffer-face-mode)
        (progn
          (visual-line-mode -1)
          (visual-fill-column-mode -1)
          (text-scale-increase 0.0)
          (buffer-face-mode -1)
          (setq-local fill-column 120))
      (visual-line-mode +1)
      (setq-local fill-column 80
                  buffer-face-mode-face '(:family "Dancing Script"))
      (buffer-face-mode +1)
      (text-scale-increase 1.5)))
  :hook (visual-line-mode . (lambda ()
                              (unless (minibufferp) (visual-fill-column-mode))))
  :custom (visual-fill-column-center-text t)
  :config
  (define-minor-mode irkalla/manuscript-mode
    "Paint our buffers with the ancient manuscript style."
    :group 'irkalla
    :global nil
    (irkalla/manuscript-toggle)))

;; :NOTE| Setup hydra's for the ever-growing bindings
(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define+ editor-hydra ()
    ("Action"
     (("z" irkalla/manuscript-mode "Manuscript Mode" :toggle t)))))

(provide 'init-fill-column)
;;; init-fill-column.el ends here
