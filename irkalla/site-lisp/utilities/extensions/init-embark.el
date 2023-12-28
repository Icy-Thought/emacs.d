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

;; :NOTE| Finally, it's time for us to define our Hydra
(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define embark-hydra
    (:title (pretty-hydra-title "──｢ Extensions: Embark ｣──" 'mdicon "nf-md-lightbulb_on_outline")
            :color teal :quit-key "q")
    ("Action(s)"
     (("a" embark-act      "Prompt -> perform")
      ("d" embark-dwim     "Run default on buffer"))
     "Documentation"
     (("h" embark-bindings "Explore Emacs bindings"))))

  (pretty-hydra-define+ main-hydra ()
    ("Extension(s)"
     (("a" embark-hydra/body "Embark")))))

(provide 'init-embark)
;;; init-embark.el ends here
