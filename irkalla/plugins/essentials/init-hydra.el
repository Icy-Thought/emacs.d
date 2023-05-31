;;; essentials/init-hydra.el -*- lexical-binding: t -*-

(defgroup irkalla-hydra '()
  "adding a helpful menu for our custom bindings"
  :tag "Irkalla Hydra"
  :group 'irkalla)

(use-package hydra
  :hook (emacs-lisp-mode . hydra-add-imenu))

(use-package hydra-posframe
  :after hydra
  :hook (after-init . hydra-posframe-enable)
  :custom
  (hydra-hint-display-type 'posframe)
  (hydra-posframe-show-params '((internal-border-width . 2)
                                (left-fringe . 15)
                                (right-fringe . 15)
                                (poshandler . posframe-poshandler-window-center))))

(use-package major-mode-hydra
  :after hydra
  :bind ("M-RET" . major-mode-hydra))

(use-package project
  :elpaca nil
  :pretty-hydra
  ((:title (pretty-hydra-title "Project Management" 'mdicon "nf-seti-project")
           :color teal :quit-key ("q" "c-g")))
  ("Finder"
   (("f" project-find-file "navigate file in project" :exit t)
    ("f" project-or-external-find-file "navigate file in project or external root" :exit t)
    ("r" projectile-recent "Navigate to recent file in project" :exit t))
   "Buffers"
   (("b" project-switch-to-buffer "Switch to buffer in project" :exit t)
    ("K" project-kill-buffers "Kill opened buffers in project" :exit t))
   "Actions"
   (("R" project-query-replace-regexp "Query-replace REGEXP for all files in project" :exit t)
    ("m" project-compile "Compile project" :exit t))
   "Modes"
   (("g" project-vc-dir "Run VC-DIR in project" :exit t)
    ("h" project-dired "Start Dired in project" :exit t)
    ("t" projectile-run-vterm "Run VTerm in project" :exit t))
   "Search"
   (("/" project-find-regexp "Find all matches for REGEXP in project" :exit t)
    ("s" project-or-external-find-regexp "Find all matches for REGEXP in project OR outside" :exit t)
    ("p" projectile-switch-project "Switch to known project" :exit t)))
  :bind ("M-RET p" . project-hydra/body))


(use-package eglot
  :elpaca nil
  :pretty-hydra
  ((:title (pretty-hydra-title "Eglot (LSP)" 'mdicon "nf-md-code_braces_box")
           :color teal :quit-key ("q" "c-g")))
  ("Find"
   (("d"  eglot-find-declaration "Find declaration for SYM" :exit t)
    ("i"  eglot-find-implementation "Find implementation for SYM" :exit t)
    ("D"  eglot-find-typeDefinition "Find type-def for SYM" :exit t))
   "Edit"
   (("r" eglot-rename "Rename symbol -> NEWNAME" :exit t)
    ("a" eglot-code-actions "Display code actions of region" :exit t))
   "Format"
   (("=" eglot-format-buffer "Format active buffer" :exit t)
    ("]" eglot-format "Format highlighted region" :exit t))
   "Management"
   (("X" eglot-shutdown "Shutdown Eglot server" :exit t)
    ("R" eglot-reconnect "Re-connect Eglot server" :exit t)
    ("E" eglot-events-buffer "Display server events buffer" :exit t)))
  :bind ("M-RET l" . eglot-hydra/body))

(provide 'init-hydra)
