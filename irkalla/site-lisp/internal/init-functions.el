;;; init-functions.el --- Useful Emacs Functions -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Writing your configuration without writing your personal functions is mad..

;;; Code:

;;;###autoload
(defun irkalla/read-secret-file (filename)
  "Fetch content of secrets file generated by agenix."
  (with-temp-buffer
    (insert-file-contents (concat "/run/agenix/" filename))
    (string-trim-right (buffer-string))))

;;;###autoload
(defun irkalla/add-public-projects ()
  "Add directories present in our /public directory to known projects."
  (interactive)
  (let ((base-dir (expand-file-name "~/Workspace/public")))
    (mapc (lambda (dir)
            (when (file-directory-p dir)
              (projectile-add-known-project dir)))
          (directory-files base-dir t "^[^.].*"))
    (message "Directories inside ~/Workspace/public added as known projects.")))

;;;###autoload
(defun irkalla/no-distractions ()
  "Toggle buffer appearance for a touch of sophistication."
  (interactive)
  (cond
    (buffer-face-mode
     (display-line-numbers-mode +1)
     (olivetti-mode -1)
     (text-scale-increase 0.0)
     (buffer-face-mode -1))
    (t (display-line-numbers-mode -1)
       (olivetti-mode +1)
       (olivetti-set-width 80)
       (text-scale-increase 1.5)
       (setq-local buffer-face-mode-face '(:family "Dancing Script"))
       (buffer-face-mode +1))))

(provide 'init-functions)
;;; init-functions.el ends here
