;;; init.el --- Initialization: Where It All Begins -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; The main file where we include our (increasing?) Emacs modules & configurations.

;;; Code:

;; Requiring `Base' modules to prevent config collision
(require 'init-elpaca)
(require 'init-functions)
(require 'init-garbage)
(require 'init-hydra)
(require 'init-frames)
(require 'init-scrolling)

;; :NOTE| Time of the day shall be in the superiour format
(use-feature time
  :custom
  (display-time-24hr-format t)
  (display-time-day-and-date t)
  (display-time-default-load-average nil))

;; Requiring the remaining modules
(require 'init-decorations)
(require 'init-utilities)
(require 'init-editor)

(provide 'init)
;;; init.el ends here
