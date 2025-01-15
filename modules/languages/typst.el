;;; typst.el --- Typst Development Environment -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-package typst-ts-mode
  :ensure (:host sourcehut :repo "meow_king/typst-ts-mode")
  :mode ("\\.typ\\'" . typst-ts-mode)
  :hook (typst-ts-mode . eglot-ensure)
  :config
  (when (executable-find "tinymist")
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs '(typst-ts-mode "tinymist"))))

  (with-eval-after-load 'consult-imenu
    (setopt consult-imenu-config
            (append consult-imenu-config
                    '((typst-ts-mode
                       :topLevel "Headings"
                       :types ((?h "Headings" typst-ts-markup-header-face)))))))
  :custom (typst-ts-mode-enable-raw-blocks-highlight t))

;; :NOTE| Adding a formatting option

(when (executable-find "typstyle")
  (with-eval-after-load 'apheleia
    (setf (alist-get 'typst-ts-mode apheleia-mode-alist) '(typstyle))))

;; :NOTE| Creating a hydra menu

(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define typst-hydra
    (:title (pretty-hydra-title "──｢ Langspec: Typst ｣──" 'mdicon "nf-md-math_compass")
     :color teal :quit-key "q")
    ("Build"
     (("c" typst-ts-mode-compile "Compile")
      ("p" typst-ts-mode-preview "Preview")
      ("u" typst-ts-mode-compile-and-preview "Run & Preview"))))

  (pretty-hydra-define+ editor-hydra ()
    ("Markup"
     (("t" (if (eq major-mode 'typst-ts-mode)
               (typst-hydra/body)
             (message "You are not in a typst buffer.")) "Typst")))))

(provide 'irkalla/typst)
