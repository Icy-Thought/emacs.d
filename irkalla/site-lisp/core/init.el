;;; init.el --- Core: Where It All Begins -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; The main file where we include our (increasing?) Emacs modules & configurations.

;;; Code:

(setq user-full-name "Icy-Thought"
      user-mail-address "icy-thought@pm.me")

(use-package time
  :elpaca nil
  :defer t
  :custom
  (display-time-24hr-format t)
  (display-time-day-and-date t))

;; Requiring our modules

(provide 'init)
;;; init.el ends here
