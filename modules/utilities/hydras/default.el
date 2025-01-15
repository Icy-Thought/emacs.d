;;; default.el --- Main Hydra Head -*- lexical-binding: t; -*-

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
      "o" launcher-hydra/body "Launcher")
     ("m" elpaca-hydra/body "Elpaca"))
    "Control"
    (("b" buffer-hydra/body "Buffer"))
    "Action"
    (("z" zone "Zooning out...")))))

;; :NOTE| Import custom hydra modules

(require 'irkalla/hydra-editor)
(require 'irkalla/hydra-elpaca)
(require 'irkalla/hydra-buffers)
(require 'irkalla/hydra-help)
(require 'irkalla/hydra-consult)
(require 'irkalla/hydra-embark)
(require 'irkalla/hydra-window)
(require 'irkalla/hydra-magit)
(require 'irkalla/hydra-dape)
(require 'irkalla/hydra-orgmode)

(provide 'irkalla/hydra-heads)
