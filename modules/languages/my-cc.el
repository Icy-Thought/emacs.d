;;; my-cc.el --- C/C++ Development Environment -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-feature c-ts-mode
  :mode (("\\.c\\'" . c-ts-mode)
         ("\\.h\\'" . c-ts-mode))
  :hook (c-ts-mode . eglot-ensure)
  :custom
  (c-ts-mode-indent-style 'k&r)
  (c-ts-mode-indent-offset tab-width))

;; :NOTE| Adding a formatting option

(when (executable-find "clang-format")
  (with-eval-after-load 'apheleia
    (let ((clang (assq 'clang-format apheleia-formatters)))
      (setcdr clang
              (append (cdr clang)
                      (unless (locate-dominating-file default-directory ".clang-format")
                        (when (and (boundp 'c-ts-mode-indent-offset) apheleia-formatters-respect-indent-level)
                          (list (format "--style={IndentWidth: %d}" c-ts-mode-indent-offset)))))))
    (setf (alist-get 'c-ts-mode apheleia-mode-alist) '(clang-format))))

;; :NOTE| Creating a hydra menu

(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define emacs-lisp-hydra
    (:title (pretty-hydra-title "──｢ Langspec: Emacs Lisp ｣──" 'sucicon "nf-custom-emacs")
            :color teal :quit-key "q")
    ("Actions"
     (("a" apropos "Show $SYMB == pattern"))))

  (pretty-hydra-define+ editor-hydra ()
    ("Programming"
     (("x" emacs-lisp-hydra/body "Emacs Lisp")))))

(provide 'my-cc)
