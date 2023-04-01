;;; ui/init-svg-tag.el -*- lexical-binding: t -*-

(defgroup irkalla-svg-tag '()
  "SVG library to intended to make our Emacs neater!"
  :tag "Irkalla SVG-Tag"
  :group 'irkalla)

(use-package svg-tag-mode
  :hook ((prog-mode org-mode markdown-mode) . svg-tag-mode)
  :config
  (setq svg-tag-tags
        '(;; Replaces any occurence of :XXX: with a dynamic SVG tag displaying XXX
          ("\\(:[A-Z]+:\\)" . ((lambda (tag)
                                 (svg-tag-make tag :face 'success :inverse t :beg 1 :end -1))))
          ;; other tags
          ("DONE:"  . ((lambda (tag) (svg-tag-make "DONE:"  :face 'fringe  :inverse t))))
          ("FIXME:" . ((lambda (tag) (svg-tag-make "FIXME:" :face 'error :inverse t))))
          ("HACK:"  . ((lambda (tag) (svg-tag-make "HACK:"  :face 'warning :inverse t))))
          ("NOTE:"  . ((lambda (tag) (svg-tag-make "NOTE:"  :face 'warning :inverse t))))
          ("TODO:"  . ((lambda (tag) (svg-tag-make "TODO:"  :face 'warning :inverse t)))))))

(provide 'init-svg-tag)
