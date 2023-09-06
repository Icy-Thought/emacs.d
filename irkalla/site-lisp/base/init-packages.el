;;; init-packages.el --- Packages & Management Configurations -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Relying on Emacs internal packages only is not enough most of the times, thus we ought to introduce a method to
;; install/customize external packages according to the aethetic standard.

;;; Code:

(setq-default package-user-dir (expand-file-name "var/elpa" user-emacs-directory)
              package-archives
              '(("elpa"   . "https://elpa.gnu.org/devel/")
                ("nongnu" . "https://elpa.nongnu.org/nongnu-devel/")
                ("melpa" . "https://melpa.org/packages/")
                ("org" . "https://orgmode.org/elpa/"))
              package-archive-priorities
              '(("elpa" . 1) ("org" . 2) ("melpa" .  3)))

;; We like to live on the edge most of the times, why limit internal package version to stable?
(setq-default package-install-upgrade-built-in t
              use-package-compute-statistics t)

;; Reducing time consumed to start Emacs
(setq-default package-enable-at-startup nil
              package--init-file-ensured t
              package-quickstart nil)

(provide 'init-packages)
;;; init-packages.el ends here
