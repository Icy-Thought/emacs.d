;;; init-whichkey.el --- WhichKey: Menu For The Lost User -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Quick-access menu which provides detailed information about the binding one has recently pressed.

;;; Code:

(use-package which-key
  :diminish which-key-mode
  :hook ((elpaca-after-init . which-key-mode)
         (which-key . which-key-setup-minibuffer))
  :custom
  (which-key-allow-evil-operators t)
  (which-key-idle-delay 0.3)
  (which-key-show-remaining-keys t)
  (which-key-separator " → ")
  (which-key-sort-order 'which-key-prefix-then-key-order))


(provide 'init-whichkey)
;;; init-whichkey.el ends here
