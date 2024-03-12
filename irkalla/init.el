;;; init.el --- Initialization: Where It All Begins -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; The main file where we include our (increasing?) Emacs modules & configurations.

;;; Code:


;; :NOTE| Time of the day shall be in the superiour format


;; Requiring the remaining modules
(require 'init-decorations)
(require 'init-utilities)
(require 'init-editor)

(provide 'init)
;;; init.el ends here
