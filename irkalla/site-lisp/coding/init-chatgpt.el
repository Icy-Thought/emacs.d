;;; init-chatgpt.el --- ChatGPT: Probabilistic Generated Answers  -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; An intelligent buttler that answers your needs not in an accurate fasion, but in a more convincing way.

;;; Code:

(use-package chatgpt-shell
  :general
  (irkalla/comma-lead-keydef
    "o c" '(chatgpt-shell :which-key "Open ChatGPT")
    "o d" '(dall-e-shell  :which-key "Open DALL-E"))
  :custom (chatgpt-shell-openai-key (lambda () (irkalla/read-secret-file "closedAI"))))

(provide 'init-chatgpt)
;;; init-chatgpt.el ends here
