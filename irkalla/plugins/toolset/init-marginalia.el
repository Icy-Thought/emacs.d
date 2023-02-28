;;; toolset/init-marginalia.el -*- lexical-binding: t -*-

(defgroup irkalla-marginalia '()
  "marks and annotations for our completions"
  :tag "Irkalla Marginalia"
  :group 'irkalla)

(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
	 :map minibuffer-local-map
	 ("M-A" . marginalia-cycle))
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

;; Adding icons to our completions
(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(provide 'init-marginalia)
