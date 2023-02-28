;;; lisp/init-decorations.el -*- lexical-binding: t -*-

(defun set-face-attributes! (face attrs)
  "Sets the specified ATTRS on the given FACE."
  (when (facep face)
    (apply #'set-face-attribute face nil attrs)))

(defun custom-set-faces! (face-specs)
  "Applies a list of FACE-SPECS in the form of (FACE . ATTRS) to set custom faces."
  (dolist (face-spec face-specs)
    (let ((face (car face-spec))
          (attrs (cdr face-spec)))
      (set-face-attributes! face attrs))))

;; Making our beloved font more aesthetically pleasing!
(custom-set-faces!
 '((default :family "VictorMono Nerd Font" :height 125 :weight semi-bold)
   (fixed-pitch :family "VictorMono Nerd Font" :height 75 :weight semi-bold)
   (variable-pitch :family "VictorMono Nerd Font" :height 75 :weight semi-bold)))

;; Change of slant style
(custom-set-faces!
 '((font-lock-builtin-face :slant italic)
   (font-lock-comment-face :slant italic)
   (font-lock-function-name-face :weight bold :slant italic)
   (font-lock-keyword-face :slant italic)))

;; (Org-Mode): Quotes shall be italic!
(setq org-fontify-quote-and-verse-blocks t)

;; (Org-Mode): Different headline sizes!
(custom-set-faces!
 '((org-document-title :height 1.2)
   (outline-1 :weight extra-bold :height 1.25)
   (outline-2 :weight bold :height 1.15)
   (outline-3 :weight bold :height 1.12)
   (outline-4 :weight semi-bold :height 1.09)
   (outline-5 :weight semi-bold :height 1.06)
   (outline-6 :weight semi-bold :height 1.03)
   (outline-8 :weight semi-bold)
   (outline-9 :weight semi-bold)))

;;(Markdown): Different headline sizes!
(custom-set-faces!
 '((markdown-header-face-1 :height 1.25 :weight extra-bold :inherit markdown-header-face)
   (markdown-header-face-2 :height 1.15 :weight bold       :inherit markdown-header-face)
   (markdown-header-face-3 :height 1.08 :weight bold       :inherit markdown-header-face)
   (markdown-header-face-4 :height 1.00 :weight bold       :inherit markdown-header-face)
   (markdown-header-face-5 :height 0.90 :weight bold       :inherit markdown-header-face)
   (markdown-header-face-6 :height 0.75 :weight extra-bold :inherit markdown-header-face)))

(provide 'init-decorations)
