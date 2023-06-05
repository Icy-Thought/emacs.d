;;; ui/init-telephone-line.el -*- lexical-binding: t -*-

(defgroup irkalla-telephone-line '()
  "A new implementation of Powerline for Emacs"
  :tag "Irkalla Telephone-Line"
  :group 'irkalla)

(use-package telephone-line
  :hook (elpaca-after-init . telephone-line-mode)
  :custom
  (telephone-line-height 26)
  (telephone-line-evil-use-short-tag t)

  ;; Left separator
  (telephone-line-primary-left-separator 'telephone-line-tan-left)
  (telephone-line-secondary-left-separator 'telephone-line-tan-hollow-left)

  ;; Right separator
  (telephone-line-primary-right-separator 'telephone-line-tan-right)
  (telephone-line-secondary-right-separator 'telephone-line-tan-hollow-right))

(provide 'init-telephone-line)
