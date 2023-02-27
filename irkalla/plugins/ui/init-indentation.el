;;; init-indentation.el -*- lexical-binding: t -*-

(defgroup irkalla-indentation '()
    "highlight our code indentations"
    :tag "Irkalla Indentation"
    :group 'irkalla)

(use-package highlight-indent-guides
    :defer t
    :hook (prog-mode . highlight-indent-guides-mode)
    :if (display-graphic-p)
    :diminish
    :config
    (setq highlight-indent-guides-method 'character
          highlight-indent-guides-responsive 'top
          highlight-indent-guides-delay 0))

;; TODO: colors -> indentation level + color change based on level of indentation

(provide 'init-indentation)
