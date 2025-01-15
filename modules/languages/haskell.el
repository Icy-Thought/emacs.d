;;; haskell.el --- Haskell Development Environment -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-package haskell-ts-mode
  :mode ("\\.hs\\(c\\|-boot\\)?\\'" . haskell-ts-mode)
  :hook (haskell-ts-mode . eglot-ensure)
  :init (derived-mode-add-parents 'haskell-ts-mode '(haskell-mode))
  :custom (haskell-ts-highlight-signature t))

;; :NOTE| Adding a formatting option

(when (executable-find "fourmolu")
  (with-eval-after-load 'apheleia
    (setf (alist-get 'haskell-ts-mode apheleia-mode-alist) '(fourmolu))))

;; :NOTE| Creating a hydra menu

(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define haskell-hydra
    (:title (pretty-hydra-title "──｢ Langspec: Haskell ｣──" 'devicon "nf-dev-haskell")
            :color teal :quit-key "q")
    ("General"
     (("r" haskell-ts-run-haskell           "Open REPL")
      ("c" haskell-ts-compile-region-and-go "Code -> REPL"))))

  ;; :TODO| add when -ts- supports features.
  ;; ("h" haskell-hoogle                  "Hoogle")
  ;; "Action"
  ;; (("C" haskell-check                  "Check")
  ;;  ("c" haskell-compile                "Compile"))
  ;; "Cabal"
  ;; (("b" haskell-process-cabal-build    "Build")
  ;;  ("B" haskell-process-cabal          "Build +Flags"))

  (pretty-hydra-define+ editor-hydra ()
    ("Programming"
     (("h" (if (memq major-mode '(haskell-ts-mode))
               (haskell-hydra/body)
             (message "You are not in a Haskell buffer.")) "Haskell")))))

(provide 'irkalla/haskell)
