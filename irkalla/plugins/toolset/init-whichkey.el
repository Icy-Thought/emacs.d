;;; toolset/init-whichkey.el -*- lexical-binding: t -*-

(use-package which-key
  :config
  (setq which-key-separator " → "
        which-key-show-early-on-C-h t
        which-key-popup-type 'minibuffer
        which-key-idle-delay 0.3))
  (which-key-mode)

(provide 'init-whichkey)
