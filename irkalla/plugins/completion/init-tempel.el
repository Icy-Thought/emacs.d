;;; completion/init-tempel.el -*- lexical-binding: t -*-

(defgroup irkalla-tempel '()
  "Simple templates for Emacs."
  :tag "Irkalla Tempel"
  :group 'irkalla)

(use-package tempel
  :hook ((prog-mode text-mode) . tempel-setup-capf)
  :init
  (setq-default tempel-path (expand-file-name "templates/*.eld" irkalla-directory))

  ;; Insert completion at cursor point!
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                    (cons #'tempel-complete
                              completion-at-point-functions))))

;; FIXME: migrate cdlatex -> tempel
;; (use-package cdlatex
;;   :init
;;   (add-hook 'latex-mode-hook #'turn-on-cdlatex))

(provide 'init-tempel)
