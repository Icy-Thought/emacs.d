;;; essentials/init-whichkey.el -*- lexical-binding: t -*-

(defgroup irkalla-whichkey '()
  "A helpful suggestion menu for our forgetful minds."
  :tag "Irkalla Which-Key"
  :group 'irkalla)

(use-package which-key
  :hook (elpaca-after-init . which-key-mode)
  :custom
  (which-key-separator " → ")
  (which-key-show-early-on-C-h t)
  (which-key-popup-type 'minibuffer)
  (which-key-idle-delay 0.3))

(provide 'init-whichkey)
