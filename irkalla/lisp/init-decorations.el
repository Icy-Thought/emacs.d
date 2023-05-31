;;; lisp/init-decorations.el -*- lexical-binding: t -*-

;; (def) Toggle frame -> transparent
(defun irkalla/toggle-frame-transparency ()
  "Toggle the frame transparency of on demand!"
  (interactive)
  (let ((alpha-value
         (if (equal (frame-parameter nil 'alpha-background) 100) 85
           100)))
    (set-frame-parameter nil 'alpha-background alpha-value)
    (add-to-list 'default-frame-alist `(alpha-background . ,alpha-value))))

;; Constant values for later re-use:
(defconst irkalla/default-font "JetBrainsMono Nerd Font")

;; Making our beloved font more aesthetically pleasing!
(set-face-attribute 'default nil
                    :family irkalla/default-font
                    :height 125
                    :weight 'semi-bold)

(set-face-attribute 'variable-pitch nil
                    :family irkalla/default-font
                    :height 105
                    :weight 'semi-bold)

(set-face-attribute 'fixed-pitch nil
                    :family irkalla/default-font
                    :height 105
                    :width 'expanded
                    :weight 'semi-bold)

(set-fontset-font "fontset-default"
                  'arabic (font-spec :family "Scheherazade New;" :size 25))

;; Turning several font styles -> italic
(custom-set-faces
 '(font-lock-builtin-face       ((t (:slant italic))))
 '(font-lock-comment-face       ((t (:slant italic))))
 '(font-lock-doc-face           ((t (:slant italic))))
 '(font-lock-function-name-face ((t (:weight bold :slant italic))))
 '(font-lock-keyword-face       ((t (:slant italic))))
 '(font-lock-preprocessor-face  ((t (:weight bold))))
 '(font-lock-string-face        ((t (:slant italic)))))

;; (Org-Mode): Quotes shall be italic!
(setq-default org-fontify-quote-and-verse-blocks t)

;; (Org-Mode): Different headline sizes!
(custom-set-faces
 '(org-document-title ((t (:height 1.5))))
 '(org-level-1        ((t (:inherit outline-1 :height 1.25))))
 '(org-level-2        ((t (:inherit outline-2 :height 1.15))))
 '(org-level-3        ((t (:inherit outline-3 :height 1.12))))
 '(org-level-4        ((t (:inherit outline-4 :height 1.09))))
 '(org-level-5        ((t (:inherit outline-5 :height 1.06)))))

;;(Markdown): Different headline sizes!
(custom-set-faces
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.25 :weight extra-bold))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.15 :weight bold))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.08 :weight bold))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.00 :weight bold))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :height 0.90 :weight bold))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :height 0.75 :weight extra-bold)))))

;; Prettier Symbols
(add-hook 'prog-mode-hook 'prettify-symbols-mode)

(setq-default prettify-symbols-alist
              '(("<-" . ?←)
                ("->" . ?→)
                ("->>" . ?↠)
                ("=>" . ?⇒)
                ("/=" . ?≠)
                ("!=" . ?≠)
                ("==" . ?≡)
                ("<=" . ?≤)
                (">=" . ?≥)
                ("=<<" . (?= (Br . Bl) ?≪))
                (">>=" . (?≫ (Br . Bl) ?=))
                ("<=<" . ?↢)
                (">=>" . ?↣)))
(setq prettify-symbols-unprettify-at-point 'right-edge)

;; :NOTE| Mini-buffer display -> completion-menu
(use-package posframe
  :defer t
  :custom (posframe-mouse-banish '(0 . 5000)))

(provide 'init-decorations)
