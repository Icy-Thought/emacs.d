;;; my-hydra-heads.el --- The Spinal Cord Connecting All The Hydra Heads -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define main-hydra
    (:title (pretty-hydra-title "──｢ Phylum Cnidaria ｣──" 'mdicon "nf-md-graph")
            :color teal :quit-key "q")
    ("Main"
     (("h" helpful-hydra/body "Helpful")
      ("o" launcher-hydra/body "Launcher")
      ("m" elpaca-hydra/body "Elpaca"))
     "Control"
     (("b" buffer-hydra/body "Buffer"))
     "Action"
     (("z" zone "Zooning out...")))))

;; :NOTE| Import custom hydra modules

(require 'my-editor-hydra)
(require 'my-elpaca-hydra)
(require 'my-buffers-hydra)
(require 'my-helpul-hydra)
(require 'my-launcher-hydra)
(require 'my-evil-hydra)
(require 'my-consult-hydra)
(require 'my-embark-hydra)
(require 'my-windows-hydra)
(require 'my-magit-hydra)
(require 'my-dape-hydra)
(require 'my-orgmode-hydra)

(provide 'my-hydra-heads)
