;;; ui/init-dashboard.el -*- lexical-binding: t -*-

(defgroup irkalla-dashboard '()
  "An extensible emacs dashboard."
  :tag "Irkalla Dashboard"
  :group 'irkalla)

(use-package dashboard
  :after nerd-icons
  :hook (dashboard-mode
         . (lambda () (setq-local frame-title-format nil))) ;; remove default title
  :init
  ;; Launch dashboard on start!
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice
        (lambda () (get-buffer-create "*dashboard*")))
  :custom
  ;; UI Customizations:
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)

  ;; Customizing setup:
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))
  (dashboard-banner-logo-title "Welcome To The Underworld, Human. - Irkalla")
  (dashboard-startup-banner (expand-file-name "dasHead.svg" user-emacs-config-directory))
  (dashboard-center-content t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info t)
  ;; (dashboard-projects-switch-function 'irkalla/switch-project-by-name) ;; :TODO| setup a projectile consult switcher
  (dashboard-week-agenda t)

  ;; Quick-Navigation buttons for our dashboard
  (dashboard-set-navigator t)
  (dashboard-navigator-buttons
   `(
     ((,(nerd-icons-octicon "nf-oct-mark_github" :height 1.2 :v-adjust 0.0)
       " Homepage"
       "Browse my personal GitHub profile home."
       (lambda (&rest _) (browse-url "https://github.com/Icy-Thought")))

      (,(nerd-icons-mdicon "nf-md-download" :height 1.2 :v-adjust 0.0)
       "Elpaca Manager"
       "Manage Irkalla Emacs packages."
       (lambda (&rest _) (elpaca-manager)))

      (,(nerd-icons-mdicon "nf-md-refresh" :height 1.2 :v-adjust 0.0)
       "Restart Emacs.."
       "Restart Irkalla Emacs instance."
       (lambda (&rest _) (restart-emacs))))))

  ;; Controlling the state of our displayed items
  (dashboard-items '((recents   . 5)
                     (bookmarks . 5)
                     (projects  . 5)
                     (agenda    . 5)
                     (registers . 5)))

  (dashboard-item-names '(("Recent Files:" . "Recently opened files:")
                          ("Agenda for today:" . "Today's agenda:")
                          ("Agenda for the coming week:" . "Agenda:"))))

(provide 'init-dashboard)
