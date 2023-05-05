;;; ui/init-dashboard.el -*- lexical-binding: t -*-

(defgroup irkalla-dashboard '()
  "An extensible emacs dashboard."
  :tag "Irkalla Dashboard"
  :group 'irkalla)

(use-package dashboard
  :after nerd-icons
  :init
  ;; Launch dashboard on start!
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda ()
                                (get-buffer-create "*dashboard*")))
  :config
  ;; Disable italics for items => cleaner!
  (set-face-italic 'dashboard-items-face nil)

  ;; Setting our logo title to italic for cursive writing :P
  (set-face-attribute 'dashboard-banner-logo-title nil
                      :weight 'bold
                      :slant 'italic)
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
  (dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
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
       "Update Emacs!"
       "Update all of Irkalla's installed packages."
       (lambda (&rest _) (package-list-packages)))

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
