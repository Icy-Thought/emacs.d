;;; embark.el --- Mini-Buffer Actions -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-package embark
  :config
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; :NOTE| Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '(,(rx bos "*Embark Collect" (| "Live" "Completions") "*" eos)
                 nil
                 (window-parameters (mode-line-format . none))))
  :custom
  (embark-prompter #'embark-completing-read-prompter)
  (embark-indicators
   '(embark-highlight-indicator
     embark-isearch-highlight-indicator)))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'irkalla/embark)
