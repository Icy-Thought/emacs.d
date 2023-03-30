;;; toolset/init-hydra.el -*- lexical-binding: t -*-

(defgroup irkalla-hydra '()
  "adding a helpful menu for our custom bindings"
  :tag "Irkalla Hydra"
  :group 'irkalla)

(use-package hydra
  :init
  (bind-key "\\" 'hydra-master/body)
  (setq hydra-hint-display-type 'posframe)
  (setq hydra-posframe-show-params
        `( :internal-border-width 2
           :internal-border-color "white"
           :left-fringe 5
           :right-fringe 5
           :poshandler posframe-poshandler-window-center))
  :config
  :hydra (hydra-master (:exit t :foreign-keys warn :hint nil)
         "
╭────────────────────┐╭────────────────────┐╭────────────────────┐╭────────────────────┐
│ Category #1        ││ Category #2        ││ Category #3        ││ Category #4        │
│────────────────────││────────────────────││────────────────────││────────────────────│
│ [_a_] Bookmarks    ││ [^h^]              ││ [_o_] Organization ││ [^v^]              │
│ [_b_] Buffers      ││ [_i_] Internet     ││ [_p_] Project      ││ [_w_] Window       │
│ [_c_] Flycheck     ││ [_j_] Jump         ││ [_q_] Exit         ││ [_x_] Shell        │
│ [_d_] Development  ││ [_k_] Spell        ││ [_r_] Register     ││ [^y^]              │
│ [_e_] Emacs        ││ [_l_] Lisp         ││ [_s_] Search       ││ [^z^]              │
│ [_f_] File         ││ [_m_] Media        ││ [_t_] Text         ││                    │
│ [_g_] Git          ││ [_n_] Narrow       ││ [^u^]              ││                    │
└────────────────────╯└────────────────────╯└────────────────────╯└────────────────────╯
        ╭─────────────────────────┐╭───────────────────┐╭─────────────────────┐
        │ [_<SPC>_]: Alt. Buffers ││ [_<ESC>_]: Quit!  ││ [_\\_]: Insert '\\' │
        └─────────────────────────╯└───────────────────╯└─────────────────────╯
"

    ("<SPC>" alternate-buffers "alternate buffers")
    ("<ESC>" nil "quit")
    ("\\"    (insert "\\") "\\")
    ("a"     hydra-bookmarks/body nil)
    ("b"     hydra-buffers/body nil)
    ("c"     hydra-eglot/body nil)
    ("d"     hydra-development/body nil)
    ("e"     hydra-emacs/body nil)
    ("f"     hydra-file/body nil)
    ("g"     hydra-git/body nil)
    ("i"     hydra-internet/body nil)
    ("j"     hydra-jump/body nil)
    ("k"     hydra-spell/body nil)
    ("l"     hydra-lisp/body nil)
    ("m"     hydra-media/body nil)
    ("n"     hydra-narrow/body nil)
    ("o"     hydra-organization/body nil)
    ("p"     hydra-project/body nil)
    ("q"     hydra-exit/body nil)
    ("r"     hydra-register/body nil)
    ("s"     hydra-search/body nil)
    ("t"     hydra-text/body nil)
    ("w"     ace-window nil)
    ("x"     hydra-system/body nil)))

(provide 'init-hydra)
