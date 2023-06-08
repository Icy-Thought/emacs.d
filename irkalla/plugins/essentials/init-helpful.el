;;; essentials/init-helpful.el -*- lexical-binding: t -*-

(defgroup irkalla-helpful '()
  "an alt. to Emacs built-in help + more contextual information."
  :tag "Irkalla Helpful"
  :group 'irkalla)

(use-package helpful
  :general
  (irkalla/space-lead-keydef
    "h"   '(:ignore t        :which-key "Helpful")
    "h k" '(helpful-key      :which-key "Describe Key")
    "h f" '(helpful-callable :which-key "Describe Function")
    "h v" '(helpful-variable :which-key "Describe Variable")
    "h C" '(helpful-command  :which-key "Describe command")
    "h F" '(helpful-function :which-key "Describe interactive functions"))

  (irkalla/comma-lead-keydef
    :keymaps 'emacs-lisp-mode-map
    "h p" '(helpful-at-point :which-key "Show help for SYMB")))

(provide 'init-helpful)
