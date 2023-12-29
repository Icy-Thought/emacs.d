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

;; :NOTE| Setup hydra's for the ever-growing bindings
(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define popper-hydra
    (:title (pretty-hydra-title "──｢ Utilities: Popper ｣──" 'mdicon "nf-md-lightbulb_on_outline")
            :color teal :quit-key "q")
    ("Action(s)"
     (("t" popper-toggle      "Un/Toggle Popup")
      ("j" popper-cycle       "Cycle Between Popup(s)")
      ("s" popper-toggle-type "Add Buf. To Popup"))))

  (pretty-hydra-define+ main-hydra ()
    ("Action"
     (("t" popper-hydra/body "Popper")))))

(provide 'init-popper)
;;; init-popper.el ends here
