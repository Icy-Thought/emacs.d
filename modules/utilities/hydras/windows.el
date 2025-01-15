;;; default.el --- Windows Hydra Head -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/


(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define window-hydra
    (:title (pretty-hydra-title "──｢ Base: Frame Management ｣──" 'mdicon "nf-md-dock_window")
            :color teal :quit-key "q")
    ("Main"
     (("t"   fontaine-set-preset     "Fontaine Preset")
      ("m"   irkalla/manuscript-mode "Manuscript Mode" :toggle t)
      ("w"   whitespace-mode         "Whitespace Mode" :toggle t))
     "Windows"
     (("f"   delete-other-windows    "Focus Window")
      ("u"   winner-undo             "Restore Old Windows")
      ("r"   winner-redo             "Redo Window Change"))
     "Popper"
     (("p"   popper-toggle           "Un/Toggle Popup")
      ("TAB" popper-cycle            "Cycle Between Popup(s)")
      ("T"   popper-toggle-type      "Add Buf. To Popup"))))

  (pretty-hydra-define+ main-hydra ()
    ("Control"
     (("w" window-hydra/body "Window")))))

(provide 'irkalla/hydra-windows)
