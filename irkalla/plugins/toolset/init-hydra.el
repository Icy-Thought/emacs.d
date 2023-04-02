;;; toolset/init-hydra.el -*- lexical-binding: t -*-

(defgroup irkalla-hydra '()
  "adding a helpful menu for our custom bindings"
  :tag "Irkalla Hydra"
  :group 'irkalla)

(use-package hydra
  :bind (("\\" . 'hydra-master/body))
  :init
  (setq hydra-hint-display-type 'posframe)
  (setq hydra-posframe-show-params
        `( :internal-border-width 2
           :internal-border-color "grey"
           :left-fringe 15
           :right-fringe 15
           :poshandler posframe-poshandler-window-center))
  :config
  (defhydra hydra-master (:exit t :foreign-keys warn :hint nil)
    "
                                ╭─────────────────────┐
                               <    Master of Hydra    >
                                └─────────────────────╯
╭────────────────────┐╭────────────────────┐╭────────────────────┐╭────────────────────┐
│ Category #1        ││ Category #2        ││ Category #3        ││ Category #4        │
│────────────────────││────────────────────││────────────────────││────────────────────│
│ [_a_] Bookmarks      ││ [^h^]                ││ [_o_] Organization   ││ [^v^]                │
│ [_b_] Buffers        ││ [_i_] Internet       ││ [_p_] Project        ││ [_w_] Window         │
│ [_d_] Development    ││ [_j_] Jump           ││ [_q_] Exit           ││ [_x_] Shell          │
│ [_e_] Eglot (LSP)    ││ [_k_] Spell          ││ [_r_] Register       ││ [^y^]                │
│ [_E_] Emacs          ││ [_l_] Lisp           ││ [_s_] Search         ││ [^z^]                │
│ [_f_] File           ││ [_m_] Media          ││ [_t_] Text           ││                    │
│ [_g_] Git            ││ [_n_] Narrow         ││ [^u^]                ││                    │
└────────────────────╯└────────────────────╯└────────────────────╯└────────────────────╯
        ╭─────────────────────────┐╭───────────────────┐╭──────────────────────┐
        │ [_<SPC>_]: Alt. Buffers   ││ [_\\_]: Insert '\\'   ││ [_<ESC>_]: Exit Hydra! │
        └─────────────────────────╯└───────────────────╯└──────────────────────╯

"
    ("<SPC>" alternate-buffers)
    ("<ESC>" nil)
    ("\\"    (insert "\\"))
    ("a"     hydra-bookmarks/body)
    ("b"     hydra-buffers/body)
    ("e"     hydra-eglot/body)
    ("d"     hydra-development/body)
    ("E"     hydra-emacs/body)
    ("f"     hydra-file/body)
    ("g"     hydra-git/body)
    ("i"     hydra-internet/body)
    ("j"     hydra-jump/body)
    ("k"     hydra-spell/body)
    ("l"     hydra-lisp/body)
    ("m"     hydra-media/body)
    ("n"     hydra-narrow/body)
    ("o"     hydra-organization/body)
    ("p"     hydra-project/body)
    ("q"     hydra-exit/body)
    ("r"     hydra-register/body)
    ("s"     hydra-search/body)
    ("t"     hydra-text/body)
    ("w"     ace-window)
    ("x"     hydra-system/body))

  (defhydra hydra-project (:exit t :foreign-keys warn :hint nil)
    "
                                        ╭───────────────────────┐
                                       <    Master of Projects   >
                                        └───────────────────────╯
┌────────────────────┐┌─────────────┐┌────────────────────┐┌──────────────────────┐┌────────────────────┐
│ Find               ││ Buffers     ││ Actions            ││ Modes                ││ Search             │
│────────────────────││─────────────││────────────────────││──────────────────────││────────────────────│
│ [_f_]: File          ││ [_b_]: Buffer ││ [_R_]: Replace       ││ [_g_]: Version Control ││ [_\/_]: Find Regexp   │
│ [_F_]: File (or Ext) ││ [_K_]: Kill   ││ [_m_]: Compile       ││ [_h_]: Dired           ││ [_s_]: Multi-Occur   │
│ [_r_]: Recent File   ││             ││                    ││ [_t_]: Term            ││ [_p_]: Switch Proj   │
└────────────────────┘└─────────────┘└────────────────────┘└──────────────────────┘└────────────────────┘
                                                                                   ╭───────────────────┐
                                                                                   │ [_q_]: Exit Hydra!  │
                                                                                   └───────────────────╯

"
    ("f" project-find-file)
    ("F" project-or-external-find-file)
    ("r" project-recentf)
    ("b" project-switch-to-buffer)
    ("K" project-kill-buffers)
    ("R" project-query-replace-regexp)
    ("m" project-compile)
    ("c" project-async-shell-command)
    ("C" project-shell-command)
    ("g" project-vc-dir)
    ("h" project-dired)
    ("t" projectile-run-vterm)
    ("\/" project-find-regexp)
    ("A" project-or-external-find-regexp)
    ("s" project-multi-occur)
    ("p" projectile-switch-project)
    ("q" nil)))

(provide 'init-hydra)
