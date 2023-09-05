;;; init-dashboard.el --- Dashboard: An Emacs Dashboard -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; A small dashboard for us to settle on when first launching Emacs.

;;; Code:

(use-package dashboard
  :after nerd-icons
  :hook (elpaca-after-init . dashboard-open)
  :custom-face
  (dashboard-heading ((t (:weight regular))))
  (dashboard-banner-logo-title ((t (:weight regular))))
  :custom
  (initial-buffer-choice (lambda ()
                           (dashboard-refresh-buffer)
                           (get-buffer dashboard-buffer-name)))

  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)

  (dashboard-banner-logo-title "Welcome To The Underworld, Human. - Irkalla")
  (dashboard-center-content t)
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))
  (dashboard-startup-banner (expand-file-name "lib/logos/owl-skull.svg" user-emacs-directory))
  (dashboard-path-max-length 20)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info t)
  ;; (dashboard-projects-switch-function 'irkalla/switch-project-by-name) ;; :TODO| setup a projectile consult switcher
  (dashboard-week-agenda t)

  (dashboard-set-navigator t)
  (dashboard-navigator-buttons
   `(
     ((,(nerd-icons-octicon "nf-oct-mark_github" :height 1.2 :v-adjust 0.0)
       "Homepage"
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

  (dashboard-items '((recents   . 5)
                     (bookmarks . 5)
                     (projects  . 5)
                     (agenda    . 5)
                     (registers . 5)))

  (dashboard-item-names '(("Recent Files:" . "Recently opened files:")
                          ("Agenda for today:" . "Today's agenda:")
                          ("Agenda for the coming week:" . "Agenda:"))))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
