;;; init.el --- Initialization: Where It All Begins -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; The main file where we include our (increasing?) Emacs modules & configurations.

;;; Code:

;; NOTE| Time to append our module to Irkalla
(defmacro irkalla/enable-modules (module-list)
  "Enable `init-' modules specified by the given MODULE-LIST."
  (if (listp module-list)
      `(progn
         ,@(mapcar (lambda (module)
                     `(require (quote ,(intern (concat "init-" (symbol-name `,module))))))
                   module-list))
    (error "Invalid argument. MODULE-LIST should be a list of symbols")))

;; Requiring `Base' modules to prevent config collision
(irkalla/enable-modules
 (elpaca functions garbage general frames scrolling))

;; :NOTE| Time of the day shall be in the superiour format
(use-package time
  :elpaca nil
  :custom
  (display-time-24hr-format t)
  (display-time-day-and-date t)
  (display-time-default-load-average nil))

;; Requiring the remaining modules
(irkalla/enable-modules
 (decorations utilities editor coding))

(provide 'init)
;;; init.el ends here
