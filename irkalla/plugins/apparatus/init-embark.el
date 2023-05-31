;;; apparatus/init-embark.el -*- lexical-binding: t -*-

(defgroup irkalla-embark '()
  "A unified interface for performing actions on various kinds of objects."
  :tag "Irkalla Embark"
  :group 'irkalla)

(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly))

(use-package embark
  :general
  (irkalla/comma-lead-keydef
    "e a" '(embark-act      :which-key "Prompt user for action -> perform")
    "e d" '(embark-dwim     :which-key "Run default action on buffer")
    "e h" '(embark-bindings :which-key "Explore all available Emacs bindings"))
  :hook (eldoc-documentation-functions . embark-eldoc-first-target)
  :custom
  (embark-prompter #'embark-completing-read-prompter)
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  :init (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-embark)
