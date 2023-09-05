;;; init-aesthetics.el --- Decoration-related Configurations -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; We cannot allow Emacs to co-exist with aesthetically modified applications without applying some form of pleasantaries to it!

;;; Code:

(defcustom irkalla/default-font-family "VictorMono Nerd Font"
  "The default font of our Irkalla Emacs."
  :type 'string
  :group 'irkalla)

;; Defining what constitutes an Emacs frame. 
(setq-default default-frame-alist
              (append (list
                       '(alpha-background . 85)
                       `(font . ,(concat irkalla/default-font-family "-13.5:weight=bold:antialias=true"))
                       '(height . 125)
                       '(mouse-color . "white")

                       ;; :NOTE| Disabling unnecessary bloat..
                       '(fullscreen . nil)
                       '(menu-bar-lines . 0)
                       '(tool-bar-lines . 0)
                       '(vertical-scroll-bars . nil))))

;; Modifying the font for our polyglotic mind..
(set-fontset-font "fontset-default"
                  'arabic (font-spec :family "Scheherazade New;" :size 25))

;;;###autoload
(defun irkalla/toggle-frame-transparency ()
  "Toggle (on/off) Emacs frame transparency on demand!"
  (interactive)
  (let ((alpha-value
         (if (equal (frame-parameter nil 'alpha-background) 100)
             85 100)))
    (set-frame-parameter nil 'alpha-background alpha-value)
    (add-to-list 'default-frame-alist `(alpha-background . ,alpha-value))))

;;;###autoload
(defun irkalla/apply-theme (palette)
  "A quicker way to apply our installed themes."
  (if (custom-theme-p palette)
      (enable-theme palette)
    (load-theme palette :no-confirm)))

;; Replace several symbols with prettier alternatives
(use-package prettify-symbols
  :elpaca nil
  ;; Use-case: lambda -> 𝛌
  :hook (emacs-lisp-mode . prettify-symbols-mode)
  :custom (prettify-symbols-unprettify-at-point 'right-edge))

(provide 'init-aesthetics)
;;; init-aesthetics.el ends here

