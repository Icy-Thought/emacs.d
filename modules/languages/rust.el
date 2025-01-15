;;; rust.el --- Rust Development Environment -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-ts-mode)
  :hook (rust-ts-mode . eglot-ensure)
  :init
  ;; :HACK| oddly enough, libraries are not being loaded...
  (require 'rust-cargo)
  (require 'rust-compile)
  (require 'rust-playpen)
  (require 'rust-rustfmt))

;; NOTE|

(use-package ob-rust :after (ob))

;; :NOTE| Adding a formatting option

(when (executable-find "rustfmt")
  (with-eval-after-load 'apheleia
    (setf (alist-get 'rust-ts-mode apheleia-mode-alist) '(rustfmt))))

;; :NOTE| Creating a hydra menu

(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define rust-hydra
    (:title (pretty-hydra-title "──｢ Langspec: Rust ｣──" 'devicon "nf-dev-rust")
            :color teal :quit-key "q")
    ("Actions"
     ((">" rust-promote-module-into-dir "Module -> Dir/mod.rs")
      ("p" rust-playpen-region          "Region -> Playground")
      ("P" rust-playpen-buffer          "Buf -> Playground"))
     "Build"
     (("r" rust-run                     "Run Project")
      ("R" rust-run-release             "Run Release")
      ("t" rust-test                    "Tests Project")
      ("l" rust-run-clippy              "Run Clippy")
      ("c" rust-compile                 "Compile")
      ("C" rust-compile-release         "Compile Release")
      ("e" rust-check                   "Check for Errors"))
     "Cargo"
     (("a" cargo-process-add            "Add")
      ("x" cargo-process-rm             "Delete")
      ("c" cargo-process-clean          "Clean")
      ("h" cargo-process-doc            "Docs")
      ("u" cargo-process-update         "Update"))))

  (pretty-hydra-define+ editor-hydra ()
    ("Programming"
     (("r" (if (memq major-mode '(rust-mode rust-ts-mode))
               (rust-hydra/body)
             (message "You are not in a rust buffer.")) "Rust")))))

(provide 'irkalla/rust)
