;;; ui/init-dashboard.el -*- lexical-binding: t -*-

(defgroup irkalla-dashboard '()
  "An extensible emacs dashboard."
  :tag "Irkalla Dashboard"
  :group 'irkalla)

(use-package dashboard
  :init
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  :custom
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))
  :config
  (setq dashboard-banner-logo-title "Welcome To The Underworld, Human. - Irkalla"
        dashboard-startup-banner "~/.config/emacs/dasHead.svg"
        dashboard-center-content t
        dashboard-item-names '(("Recent Files:" . "Recently opened files:")
                               ("Agenda for today:" . "Today's agenda:")
                               ("Agenda for the coming week:" . "Agenda:"))
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-set-navigator t
        dashboard-set-init-info t
        dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name
        dashboard-week-agenda t))


(provide 'init-dashboard)
