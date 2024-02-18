;;; init-typeface.el --- Typeface Customizations -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Modifying the our typeface to achieve a better reading environment is necessary!

;;; Code:

(use-package fontaine
  :demand t
  :hook (kill-emacs . fontaine-store-latest-preset)
  :custom
  (fontaine-presets `((default) ;; <- fallback values
                      (reading
                       :variable-pitch-family "Amita"
                       :variable-pitch-height 175
                       :variable-pitch-weight normal)
                      (large
                       :default-weight semibold
                       :default-height 180
                       :bold-weight extrabold)
                      (t
                       :default-family "VictorMono Nerd Font"
                       :default-height 145
                       :default-weight semibold
                       :bold-weight bold
                       :italic-weight semibold-italic
                       :fixed-pitch-family "VictorMono Nerd Font Mono"
                       :fixed-pitch-height 145
                       :variable-pitch-family nil
                       :variable-pitch-height 1.00)))
  :config
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'default))

  (defun irkalla/manuscript-toggle ()
    "Toggle buffer appearance for a touch of sophistication."
    (if (eq (symbol-value 'fontaine-current-preset) 'regular)
        (fontaine-set-preset 'reading)
      (fontaine-set-preset 'regular)))

  (define-minor-mode irkalla/manuscript-mode
    "Paint our buffers with the ancient manuscript style."
    :group 'irkalla
    :global nil
    (irkalla/manuscript-toggle)))

(use-feature face-remap
  :hook (text-mode . variable-pitch-mode)
  :bind (("C-0" . (lambda () (interactive) (text-scale-increase 0.0)))
         ("C-+" . (lambda () (interactive) (text-scale-increase 0.5)))
         ("C--" . (lambda () (interactive) (text-scale-decrease 0.5))))
  :config
  (set-fontset-font t 'arabic (font-spec :family "Scheherazade New") nil 'prepend)
  (set-fontset-font t 'han    (font-spec :family "Sarasa Mono CL")   nil 'prepend)
  (set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") nil 'append))

(use-feature font-lock
  :custom-face
  (font-lock-builtin-face       ((t (:slant italic))))
  (font-lock-comment-face       ((t (:slant italic))))
  (font-lock-doc-face           ((t (:slant italic))))
  (font-lock-function-name-face ((t (:slant italic :weight bold))))
  (font-lock-keyword-face       ((t (:slant italic))))
  (font-lock-preprocessor-face  ((t (:weight bold))))
  (font-lock-string-face        ((t (:slant italic)))))

(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define+ window-hydra ()
    ("Main"
     (("t" fontaine-set-preset "Fontaine Preset")
      ("m" irkalla/manuscript-mode "Manuscript Mode" :toggle t)))))

(provide 'init-typeface)
;;; init-typeface.el ends here
