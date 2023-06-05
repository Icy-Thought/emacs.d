;;; essentials/init-whichkey.el -*- lexical-binding: t -*-

(defgroup irkalla-whichkey '()
  "A helpful suggestion menu for our forgetful minds."
  :tag "Irkalla Which-Key"
  :group 'irkalla)

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :custom
  (which-key-allow-evil-operators t)
  (which-key-idle-delay 0.3)
  (which-key-show-remaining-keys t)
  (which-key-separator " → ")
  (which-key-sort-order 'which-key-prefix-then-key-order)
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))

(provide 'init-whichkey)
