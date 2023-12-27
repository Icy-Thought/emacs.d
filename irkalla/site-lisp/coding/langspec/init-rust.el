;;; init-rust.el --- Langserv: Rust -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Language server configurations for Rust language.

;;; Code:

(use-package rust-mode
  :mode ("\\.rs$" . rust-mode)
  :preface
  (defun irkalla/locate-cargo-toml (dir)
    "Locate the missing Rust project Cargo.toml."
    (if-let ((root (locate-dominating-file dir "Cargo.toml")))
        (list 'vc 'Git root)))
  :hook ((rust-mode rust-ts-mode) . (lambda ()
                                      (eglot-ensure)
                                      (indent-tabs-mode -1)
                                      (add-to-list 'project-find-functions #'irkalla/locate-cargo-toml)))
  :config
  (with-eval-after-load 'eglot
    (when (executable-find "rust-analyzer")
      (add-to-list 'eglot-server-programs
                   `((rust-mode rust-ts-mode) . ("rust-analyzer"
                                                 ;; https://rust-analyzer.github.io/manual.html
                                                 :initializationOptions ((:cargo       (:features "all"))
                                                                         (:completion  (:callable (:snippets "fill_arguments")))
                                                                         (:checkOnSave (:command "clippy" :allTargets :json-false))))))))

  ;; :NOTE| apheleia formatting support
  (with-eval-after-load 'apheleia-formatters
    (when (executable-find "rustfmt")
      (setf (alist-get 'rustfmt apheleia-formatters)
            '("rustfmt" "--quiet" "--emit" "stdout"))
      (add-to-list 'apheleia-mode-alist '((rust-mode rust-ts-mode) . rustfmt)))))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode)
  :custom (cargo-process--command-clippy "clippy"))

;; :NOTE| adding org-babel support for Rust
(use-package ob-rust
  :requires (ob))

;; :NOTE| Finally, it's time for us to define our Hydra
(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define rust-hydra
    (:title (pretty-hydra-title "──｢ Langspec: Rust ｣──" 'devicon "nf-dev-rust")
            :color teal :quit-key "q")
    ("Interactive"
     (("l" rust-run-clippy     "cargo clippy")
      ("r" rust-run            "project")
      ("t" rust-test           "tests on project"))
     "Build"
     (("c" rust-compile        "Compile project")
      ("d" rust-check          "Compile & Check"))
     "Process"
     (("a" cargo-process-add   "Add")
      ("c" cargo-process-clean "Clean")))))

(provide 'init-rust)
;;; init-rust.el ends here
