;;; my-hydra.el --- Growing Quick-Access Menu -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-package pretty-hydra
  :config
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    (let ((face (or face `(:inherit hydra-face-pink :height 1.2 :slant italic)))
          (height (or height 1.2))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (display-graphic-p) icon-type icon-name)
         (let ((f (intern (format "nerd-icons-%s" icon-type))))
           (when (fboundp f)
             (concat (apply f (list icon-name :face face :height height :v-adjust v-adjust))
                     "  "))))
       (propertize title 'face face)))))

(use-package hydra-posframe
  :vc (:url "https://github.com/Ladicle/hydra-posframe")
  :after (pretty-hydra)
  :config (hydra-posframe-enable)
  :custom
  (hydra-posframe-border-width 2)
  (hydra-posframe-parameters '((left-fringe . 25) (right-fringe . 25))))

;; :NOTE| Import custom hydra modules

(require 'my-hydra-heads)

(provide 'my-hydra)
