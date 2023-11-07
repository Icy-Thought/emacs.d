;;; init-rust.el --- Langserv: Rust -*- lexical-binding: t -*-

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
  :general
  (irkalla/comma-lead-keydef rust-mode-map
    "r"   '(:ignore t       :which-key "Rust")
    "r b" '(rust-compile    :which-key "Compile project")
    "r c" '(rust-check      :which-key "Compile + cargo check")
    "r l" '(rust-run-clippy :which-key "Run cargo clippy")
    "r r" '(rust-run        :which-key "Run project")
    "r t" '(rust-test       :which-key "Run tests on project"))
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `((rust-mode rust-ts-mode)
                   . ("rust-analyzer"
                      ;; https://rust-analyzer.github.io/manual.html
                      :initializationOptions ((:cargo       (:features "all"))
                                              (:completion  (:callable (:snippets "fill_arguments")))
                                              (:checkOnSave (:command "clippy" :allTargets :json-false))))))))

;; :NOTE| adding proper cargo support
(use-package cargo
  :hook (rust-mode . cargo-minor-mode)
  :general
  (irkalla/comma-lead-keydef rust-mode-map
    "r p"   '(:ignore t           :which-key "Cargo")
    "r p a" '(cargo-process-add   :which-key "Cargo Add")
    "r p c" '(cargo-process-clean :which-key "Cargo Clean"))
  :custom (cargo-process--command-clippy "clippy"))

;; :NOTE| adding org-babel support for Rust
(use-package ob-rust
  :requires (ob))

;; :NOTE| apheleia formatting support
(with-eval-after-load 'apheleia
  (when (executable-find "rustfmt")
    (setf (alist-get 'rustfmt apheleia-formatters)
          '("rustfmt" "--quiet" "--emit" "stdout"))
    (add-to-list 'apheleia-mode-alist '((rust-mode rust-ts-mode) . alejandra))))

(provide 'init-rust)
;;; init-rust.el ends here
