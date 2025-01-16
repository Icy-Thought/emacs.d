;;; my-editor-hydra.el --- Editor Hydra Head -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define editor-hydra
    (:title (pretty-hydra-title "──｢ Chrysaora Melanaster ｣──" 'mdicon "nf-md-graph_outline")
            :color teal :quit-key "q")
    ("Programming"
     (("RET" (if (derived-mode-p 'prog-mode)
                 (call-interactively #'project-compile)
               (message "Buffer /= PROG buffer...")) "Compile")
      ("!" apheleia-mode "Fmt On Save" :toggle t))
     "Action"
     (("=" apheleia-format-buffer "Buf. Format")
      ("c" combobulate                    "Combobulate")
      (";" evil-nerd-commenter-hydra/body "Comment")
      ("b" eval-buffer                    "Eval Buf.")
      ("y" irkalla/copy-to-sysclip        "Yank -> Sys-Clip")
      ("p" irkalla/paste-from-sysclip     "Paste <- Sys-Clip"))
     "Control"
     (("l" (if (eglot-managed-p)
               (eglot-hydra/body)
             (message "You are not in an Eglot buffer.")) "Eglot (LSP)"))))

  (pretty-hydra-define visual-editor-hydra
    (:title (pretty-hydra-title "──｢ (Visual) Chrysaora Melanaster ｣──" 'mdicon "nf-md-graph_outline")
            :color teal :quit-key "q")
    ("Action"
     (("e" eval-region                    "Eval Region")
      (";" evil-nerd-commenter-hydra/body "Comment")
      ("a" ialign                         "RegEXP Alignment")
      ("y" irkalla/copy-to-sysclip        "Yank -> Sys-Clip"))
     "Navigation"
     ((")" er/expand-region               "Increase -> semantic units")
      ("(" er/contract-region             "Contract -> PREV size"))))

  (pretty-hydra-define eglot-hydra
    (:title (pretty-hydra-title "──｢ Coding: Eglot ｣──" 'faicon "nf-fa-code")
            :color teal :quit-key "q")
    ("Actions"
     (("a" eglot-code-actions    "Perform code-actions")
      ("r" eglot-rename          "Rename $SYMB"))
     "Look-up"
     (("?" xref-find-references  "Find -> references")
      ("f" xref-find-definitions "Find -> definition")
      ("/" xref-find-apropos     "Find $SYMB <- pattern")))))

;; :NOTE| Keybindings for the newly created Hydras

(with-eval-after-load 'evil
  (evil-global-set-key 'normal (kbd "SPC") 'main-hydra/body)
  (evil-global-set-key 'normal (kbd ",") 'editor-hydra/body)
  (evil-global-set-key 'visual (kbd ",") 'visual-editor-hydra/body))

(provide 'my-editor-hydra)
