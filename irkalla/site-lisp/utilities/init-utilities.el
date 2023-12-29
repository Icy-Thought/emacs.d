;;; init-utilities.el --- Utilities-related Changes -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Tools that could improve our workflow are always welcomed!

;;; Code:

(use-package alert
  :custom (alert-default-style 'libnotify))

(use-package emacs-everywhere
  :custom (emacs-everywhere-copy-command (list "cat" "%f" "|" "cb" "copy")))

(use-package helpful
  :demand t
  :pretty-hydra
  ((:title (pretty-hydra-title "──｢ Utilities: Helpful ｣──" 'mdicon "nf-md-help_network")
           :color teal :quit-key "q")
   ("Help"
    (("k" helpful-key      "Key(s)")
     ("f" helpful-function "Function(s)")
     ("F" helpful-callable "Interactive function(s)")
     ("v" helpful-variable "Variable(s)")
     ("c" helpful-command  "Command(s)")
     ("p" helpful-at-point "SYMB at point"))))
  :config
  (setq-default help-window-select t)

  ;; :NOTE| Setup hydra's for the ever-growing bindings
  (with-eval-after-load 'pretty-hydra
    (pretty-hydra-define+ main-hydra ()
      ("Extension"
       (("h" helpful-hydra/body "Helpful"))))))

(use-package screenshot
  :elpaca (:host github :repo "tecosaur/screenshot"))

;; :NOTE| Import the custom modules
(irkalla/enable-modules
 (popper tabspaces tab-bar direnv dired whichkey dashboard readers consult embark magit vertico org-roam marginalia
         recentf social terminal browser))

(provide 'init-utilities)
;;; init-utilities.el ends here
