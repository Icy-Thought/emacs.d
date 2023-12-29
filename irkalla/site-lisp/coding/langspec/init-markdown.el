;;; init-haskell.el --- Markdown -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; A widely used and very minimalistic documentation format.

;;; Code:

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :custom-face
  (markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.25 :weight extra-bold))))
  (markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.15 :weight bold))))
  (markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.08 :weight bold))))
  (markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.00 :weight bold))))
  (markdown-header-face-5 ((t (:inherit markdown-header-face :height 0.90 :weight bold))))
  (markdown-header-face-6 ((t (:inherit markdown-header-face :height 0.75 :weight extra-bold))))
  :custom (markdown-command "multimarkdown"))

;; :NOTE| Setup hydra's for the ever-growing bindings
(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define markdown-hydra
    (:title (pretty-hydra-title "──｢ Langspec: Markdown ｣──" 'devicon "nf-dev-markdown")
            :color teal :quit-key "q")
    ("Interactive"
     (("d" markdown-do "Perform -> action"))))

  (pretty-hydra-define+ langspec-hydra ()
    ("Markup"
     (("m" (if (eq major-mode 'markdown-mode)
               (markdown-hydra/body)
             (message "You are not in a markdown buffer.")) "Markdown")))))

(provide 'init-markdown)
;;; init-markdown.el ends here
