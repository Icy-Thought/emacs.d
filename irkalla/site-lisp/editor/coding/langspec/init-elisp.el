;;; init-elisp.el --- Langserv: Emacs-Lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Language server configurations for Emacs-Lisp.

;;; Code:

(use-package parinfer-rust-mode
  :hook (emacs-lisp-mode . parinfer-rust-mode)
  :custom
  (parinfer-rust-auto-download t)
  (parinfer-rust-library-directory (no-littering-expand-var-file-name "parinfer-rust/")))

;; :NOTE| Setup hydra's for the ever-growing bindings
(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define emacs-lisp-hydra
    (:title (pretty-hydra-title "──｢ Langspec: Emacs Lisp ｣──" 'sucicon "nf-custom-emacs")
            :color teal :quit-key "q")
    ("Actions"
     (("a" apropos "Show $SYMB == pattern"))))

  (pretty-hydra-define+ editor-hydra ()
    ("Programming"
     (("x" emacs-lisp-hydra/body "Emacs Lisp")))))

(provide 'init-elisp)
;;; init-elisp.el ends here
