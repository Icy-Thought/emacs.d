;;; ui/init-dashboard.el -*- lexical-binding: t -*-

(defgroup irkalla-dashboard '()
    "An extensible emacs dashboard."
    :tag "Irkalla Dashboard"
    :group 'irkalla)

(use-package dashboard
  :init
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  :config
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard"
        ;; dashboard-startup-banner "~/Pictures/Profile/omega.png"
        dashboard-center-content t
        dashboard-item-names '(("Recent Files:" . "Recently opened files:")
                               ("Agenda for today:" . "Today's agenda:")
                               ("Agenda for the coming week:" . "Agenda:"))
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-set-navigator t
        dashboard-set-init-info t
        dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name
        dashboard-week-agenda t)
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book"))))

;; TODO:
;; 1. Title face font to be italic
;; 2. Launch if buffer close -> no buffer to replace.

(provide 'init-dashboard)
