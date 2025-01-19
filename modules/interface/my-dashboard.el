;;; my-dashboard.el --- Emacs Startup Buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

;; :WARN| Without dashboard -> startup-time < 0.3Xs.

(use-package dashboard
  :after (nerd-icons)
  :hook (after-init . dashboard-refresh-buffer)
  :config
  (setopt initial-buffer-choice (lambda () (get-buffer dashboard-buffer-name)))
  :custom
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-banner-logo-title "Welcome To The Underworld, Human. - Irkalla")
  (dashboard-center-content t)
  (dashboard-modify-heading-icons '((recents . "file-text") (bookmarks . "book")))
  (dashboard-startup-banner (expand-file-name "logos/png/lotus.png" irkalla/underworld))
  (dashboard-path-max-length 20)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info t)
  (dashboard-week-agenda t)
  (dashboard-set-navigator t)
  (dashboard-items '((recents   . 5)
                     (bookmarks . 5)))
  (dashboard-item-names
   '(("Recent Files:" . " Recently opened files:")
     ("Bookmarks:"    . " Pinned Items:"))))

(provide 'my-dashboard)
