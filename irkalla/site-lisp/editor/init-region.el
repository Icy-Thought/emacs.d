;;; init-region.el --- Regional-related Customizations -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Indentations, region selection and more changes are to be displaced into this folder.

;;; Code:

(use-package ialign
  :commands (ialign))

(use-package indent-bars
  :elpaca (:host github :repo "jdtsmith/indent-bars")
  :hook (prog-mode . indent-bars-mode)
  :custom (indent-bars-zigzag nil))

(use-package expand-region
  :commands (er/expand-region er/contract-region))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package parens
  :elpaca nil
  :hook (prog-mode . show-paren-mode)
  :custom
  (show-paren-style 'parenthesis)
  (show-paren-when-point-in-periphery nil)
  (show-paren-when-point-inside-paren nil))

(use-package subword
  :elpaca nil
  :hook ((prog-mode text-mode) . subword-mode))

;; :NOTE| Setup hydra's for the ever-growing bindings
(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define visual-region-hydra
    (:title (pretty-hydra-title "──｢ Editor: Region (Visual) ｣──" 'mdicon "nf-md-vector_rectangle")
            :color teal :quit-key "q")
    ("Alignment"
     (("a" ialign "Align with REGEXP"))
     "Selection"
     ((")" er/expand-region   "Increase by semantic units")
      ("(" er/contract-region "Contract to PREV size")))))

(provide 'init-region)
;;; init-region.el ends here
