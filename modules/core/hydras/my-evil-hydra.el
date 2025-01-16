;;; my-evil-hydra.el --- Evil Hydra Head -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define evil-nerd-commenter-hydra
    (:title (pretty-hydra-title "──｢ Editing: Evil Commenting ｣──" 'faicon "nf-fa-code")
            :color teal :quit-key "q")
    ("Actions"
     (("c" evilnc-copy-and-comment-lines                 "Copy & Comment")
      ("l" evilnc-comment-or-uncomment-lines             "Un/Comment Lines")
      ("p" evilnc-comment-or-uncomment-paragraphs        "Un/Comment Paragraph"))
     "Fancy"
     (("b" evilnc-comment-box                            "Boxed Comment")
      ("l" evilnc-comment-or-uncomment-lines             "Un/Comment Lines")
      ("p" evilnc-comment-or-uncomment-paragraphs        "Un/Comment Paragraph"))
     "Fancy"
     (("b" evilnc-comment-box                            "Boxed Comment")
      ("n" evilnc-quick-comment-or-uncomment-to-the-line "Comment -> Nth Line")))))

(provide 'my-evil-hydra)
