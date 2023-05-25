;;; apparatus/init-embark.el -*- lexical-binding: t -*-

(defgroup irkalla-embark '()
  "A unified interface for performing actions on various kinds of objects."
  :tag "Irkalla Embark"
  :group 'irkalla)

(use-package embark
  :general
  (general-nmap
    ("C-."   '(embark-act      :which-key "Prompt user for action -> perform"))
    ("C-;"   '(embark-dwim     :which-key "Run default action on buffer"))
    ("C-h B" '(embark-bindings :which-key "Explore all available Emacs bindings")))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-embark)
