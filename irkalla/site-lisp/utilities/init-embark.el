;;; init-embark.el --- Embark: Mini-Buffer Actions -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Many actions made available to the user with the intentions of reducing time-consumed to execute buf. tasks.

;;; Code:

(use-package embark
  :hook (eldoc-documentation-functions . embark-eldoc-first-target)
  :general
  (irkalla/comma-lead-keydef
    "e"   '(:ignore t       :which-key "Embark")
    "e a" '(embark-act      :which-key "Prompt for action -> perform")
    "e d" '(embark-dwim     :which-key "Run default action -> buffer")
    "e h" '(embark-bindings :which-key "Explore Emacs bindings"))
  :custom
  (embark-prompter #'embark-completing-read-prompter)
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  :config
  (setopt prefix-help-command #'embark-prefix-help-command)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :requires (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-embark)
;;; init-embark.el ends here
