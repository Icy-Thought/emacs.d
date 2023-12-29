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
  :custom (chatgpt-shell-openai-key (lambda () (irkalla/read-secret-file "closedAI"))))

;; :NOTE| Setup hydra's for the ever-growing bindings
(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define+ editor-hydra ()
    ("ChatGPT"
     (("c" chatgpt-shell "Shell")
      ("d" dall-e-shell  "Dall-E")))))

(provide 'init-chatgpt)
;;; init-chatgpt.el ends here
