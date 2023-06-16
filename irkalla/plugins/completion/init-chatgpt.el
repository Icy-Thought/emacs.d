;;; apparatus/init-chatgpt.el -*- lexical-binding: t -*-

(defgroup irkalla-chatgpt '()
  "A good guessing algorithm for the lazy mind. -closedAI"
  :tag "Irkalla ChatGPT"
  :group 'irkalla)

(use-package chatgpt-shell
  :general (irkalla/comma-lead-keydef
             "o c" '(chatgpt-shell :which-key "Startup ChatGPT client")
             "o d" '(dall-e-shell  :which-key "Startup DALL-E client"))
  :custom (chatgpt-shell-openai-key (lambda ()
                                      (irkalla/read-secret-file "closedAI"))))

(provide 'init-chatgpt)
