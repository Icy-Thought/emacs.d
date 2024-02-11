;;; init-decorations.el --- Decoration-related Configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; We cannot allow Emacs to co-exist with aesthetically modified applications without applying some form of pleasantaries to it!

;;; Code:

(use-package autothemer
  :demand t
  :preface
  (defun irkalla/setup-appearance ()
    (let ((theme-name 'catppuccin-mocha))
      (when (daemonp)
        (add-hook 'after-make-frame-functions (lambda (frame)
                                                (select-frame frame)
                                                (load-theme theme-name :no-confirm))))
      (load-theme theme-name :no-confirm)))
  :config (irkalla/setup-appearance))

;; :NOTE| Replace several symbols with prettier alternatives
(use-package prettify-symbols
  :ensure nil
  :hook (emacs-lisp-mode . prettify-symbols-mode)
  :custom (prettify-symbols-unprettify-at-point 'right-edge))

;; :NOTE| SVG-library to enrich the user-experience!
(use-package svg-lib
  :preface
  (defun first-graphical-frame ()
    (remove-hook 'focus-in-hook #'first-graphical-frame-hook-function)
    (provide 'irkalla/frame-gui))
  :hook (focus-in . first-graphical-frame)
  :config
  (with-eval-after-load 'irkalla/frame-gui
    (setopt svg-lib-style-default (svg-lib-style-compute-default)
            svg-lib-style-default (plist-put svg-lib-style-default :font-size 13))))

;; :NOTE| Import the custom modules
(irkalla/enable-modules
 (typeface icons svg-tags modeline))

(provide 'init-decorations)
;;; init-decorations.el ends here
