;;; editor/init-indent.el -*- lexical-binding: t -*-

(defgroup irkalla-indent'()
  "highlight our code indentations"
  :tag "Irkalla Indent"
  :group 'irkalla)

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top))

;; TODO: colors -> indentation level + color change based on level of indentation

(provide 'init-indent)
