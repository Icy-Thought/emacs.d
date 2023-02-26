;;; init-transparency.el -*- lexical-binding: t -*-

;; We don't always want our frames to be transparent, do we?
(defun toggle-window-transparency ()
  "Disable window transparency on demand."
  (interactive)
  (let ((alpha-transparency 85))
    (pcase (frame-parameter nil 'alpha-background)
           (alpha-transparency (set-frame-parameter nil 'alpha-background 100))
           (t (set-frame-parameter nil 'alpha-background alpha-transparency)))))

(provide 'init-transparency)
