;;; init.el --- Initialization: Where It All Begins -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; The main file where we include our (increasing?) Emacs modules & configurations.

;;; Code:

;; :NOTE| Throwaway name? Perhaps. Who am I really?
(setq user-full-name "Icy-Thought"
      user-mail-address "icy-thought@pm.me")

;; :NOTE| frame fontification is necessary!
(defmacro irkalla/set-fontset (charset font-name &rest params)
  "Define font for CHARSET -> FONT-NAME + PARAMS when == existant."
  `(when (member ,font-name (font-family-list))
     (set-fontset-font "fontset-default" ,charset
                       (font-spec :family ,font-name ,@params) nil 'prepend)))

(irkalla/set-fontset 'arabic "Scheherazade New" :size 19 :weight 'normal)
(irkalla/set-fontset 'han    "Sarasa Gothic CL" :size 19 :weight 'normal)
(irkalla/set-fontset 'symbol "Noto Color Emoji")

(add-to-list 'default-frame-alist
             '(font . "VictorMono Nerd Font:size=17:weight=semibold:antialias=true"))

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
  (display-time-day-and-date t))

;; Requiring the remaining modules
(irkalla/enable-modules
 (completion decorations editor utilities))

(provide 'init)
;;; init.el ends here
