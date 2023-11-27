;;; init-annotations.el --- Annotations For the Lost -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Providing annotations for the many different parts of Emacs is never a bad thing. 

;;; Code:

(use-package marginalia
  :hook (elpaca-after-init . marginalia-mode)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right))

(provide 'init-annotations)
;;; init-annotations.el ends here
