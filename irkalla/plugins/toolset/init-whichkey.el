(use-package which-key
  :init
  (which-key-mode)
  :custom
  (which-key-separator " → " )
  (which-key-show-early-on-C-h t)
  (which-key-setup-minibuffer)
  (which-key-idle-delay 10000)
  (which-key-idle-secondary-delay 0.05))
