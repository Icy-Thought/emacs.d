(irkalla/space-lead-keydef
  "h"   '(:ignore t        :which-key "Helpful")
  "h k" '(helpful-key      :which-key "Key")
  "h f" '(helpful-callable :which-key "Function")
  "h v" '(helpful-variable :which-key "Variable")
  "h C" '(helpful-command  :which-key "Command")
  "h F" '(helpful-function :which-key "Interactive functions"))

(irkalla/comma-lead-keydef emacs-lisp-mode-map
  "h"   '(:ignore t        :which-key "Helpful")
  "h p" '(helpful-at-point :which-key "Show help for SYMB"))
