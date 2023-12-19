;;; init-popper.el --- Popper: Quick Access Pop-up Windows -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Popper reduces the burden of reaching out to buffers and automates their placement in a nice fashion.

;;; Code:

(use-package popper
  :hook (elpaca-after-init . (lambda ()
                               (popper-mode +1)
                               (popper-echo-mode +1)))
  :general
  (irkalla/space-lead-keydef
    "t"   '(:ignore t          :which-key "Popper")
    "t t" '(popper-toggle      :which-key "Un/Toggle Popup")
    "t j" '(popper-cycle       :which-key "Cycle Between Popup(s)")
    "t-s" '(popper-toggle-type :which-key "Add Buf. To Popup"))
  :custom
  (popper-reference-buffers
   '("\\*Messages\\*"
     "Output\\*$"
     "\\*Async Shell Command\\*"
     help-mode
     compilation-mode

     ;; :NOTE| terminal-related
     "^\\*eat.*\\*$"    eat-mode
     "^\\*eshell.*\\*$" eshell-mode
     "^\\*shell.*\\*$"  shell-mode
     "^\\*term.*\\*$"   term-mode
     "^\\*vterm.*\\*$"  vterm-mode))

  (popper-group-function #'popper-group-by-projectile)
  (popper-display-function #'display-buffer-full-frame))

(provide 'init-popper)
;;; init-popper.el ends here
