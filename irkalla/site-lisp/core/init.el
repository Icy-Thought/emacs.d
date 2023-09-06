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

(defmacro irkalla/enable-modules (module-list)
  "Enable 'init-' modules specified by the given MODULE-LIST."
  (if (listp module-list)
      `(progn
         ,@(mapcar (lambda (module)
                     `(require (quote ,(intern (concat "init-" (symbol-name `,module))))))
                   module-list))
    (error "Invalid argument. MODULE-LIST should be a list of symbols")))

;; Requiring `Core' modules to prevent config collision
(irkalla/enable-modules (packages elpaca functions performance options backup
                         general frames scrolling))

(use-package time
  :elpaca nil
  :custom
  (display-time-24hr-format t)
  (display-time-day-and-date t))

;; Requiring the remaining modules
(irkalla/enable-modules (aesthetics font-faces icons svg-tags))

(irkalla/enable-modules (completion corfu tempel diagnostics apheleia eldoc))

(irkalla/enable-modules (editor evil dirvish ligatures memorandum ediff region citar whitespace))

(irkalla/enable-modules (utilities garbage direnv whichkey dashboard
                         centaur modeline consult embark vc readers vertico annotations
                         recentf benchmark social terminals))

(provide 'init)
;;; init.el ends here
