;;; init-eshell.el --- Emacs SHell -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; An Emacs native SHell for our terminal related work.

;;; Code:

(use-package eshell
  :elpaca nil
  :preface
  (defun shortened-path (path max-len)
    (require 'cl-lib)
    (let* ((components (split-string (abbreviate-file-name path) "/"))
           (len (+ (1- (length components))
                   (cl-reduce '+ components :key 'length)))
           (str ""))
      (while (and (> len max-len) (cdr components))
        (setq str (concat str (if (= 0 (length (car components)))
                                  "/" (string (elt (car components) 0) ?/)))
              len (- len (1- (length (car components))))
              components (cdr components)))
      (concat str (cl-reduce (lambda (a b) (concat a "/" b)) components))))

  (defun irkalla/eshell-prompt ()
    (concat
     (propertize (concat "  " (shortened-path (eshell/pwd) 40)) 'face 'font-lock-constant-face)
     (when (package-installed-p 'magit)
       (propertize (if (magit-get-current-branch)
                       (concat "   " (magit-get-current-branch)) "" 'face 'font-lock-variable-name-face)))
     (when (package-installed-p 'envrc)
       (propertize (if (string= envrc--status 'none)
                       "" "   " 'face 'font-lock-string-face)))
     (propertize (concat "   " (format-time-string "%H:%M" (current-time))) 'face 'font-lock-variable-name-face) 
     (propertize "\n └─➤ 𝝺 " 'face 'font-lock-type-face))) 
  :custom
  (eshell-error-if-no-glob t)
  (eshell-hist-ignoredups t)
  (eshell-save-history-on-exit t)
  (eshell-scroll-to-bottom-on-input 'this)
  (eshell-scroll-to-bottom-on-output nil)
  (eshell-destroy-buffer-when-process-dies t)
  ;; :NOTE| Aesthetics of our semi-cursed prompt?
  (eshell-prompt-function #'irkalla/eshell-prompt)
  (eshell-prompt-regexp "^.*└─➤ 𝝺 ")) ;; Match last output of prompt -> prevents ~read-only~

(provide 'init-eshell)
;;; init-eshell.el ends here
