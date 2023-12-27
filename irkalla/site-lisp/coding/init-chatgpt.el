;;; init-chatgpt.el --- ChatGPT: Probabilistic Generated Answers  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; An intelligent buttler that answers your needs not in an accurate fasion, but in a more convincing way.

;;; Code:

(use-package chatgpt-shell
  :commands (chatgpt-shell dall-e-shell)
  :pretty-hydra
  ((:title (pretty-hydra-title "──｢ Coding: ChatGPT ｣──" 'devicon "nf-dev-code")
           :color teal :quit-key "q")
   ("Interactive"
    (("c" chatgpt-shell "ChatGPT")
     ("d" dall-e-shell  "Dall-E"))))
  :custom (chatgpt-shell-openai-key (lambda () (irkalla/read-secret-file "closedAI"))))

(provide 'init-chatgpt)
;;; init-chatgpt.el ends here
