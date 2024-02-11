;;; init-babel.el --- Org-Babel: Language Suppoert for Org-Mode -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; The superior Jupyter Notebooks workflow with multiple language support. (all?)

;;; Code:

(use-package ob
  :ensure nil
  :preface
  ;; :NOTE| https://emacs.stackexchange.com/a/20618
  (defun demand-babel-languages (orig-fun &rest args)
    "Load language if needed before executing a source block."
    (let ((language (org-element-property :language (org-element-at-point))))
      (unless (cdr (assoc (intern language) org-babel-load-languages))
        (add-to-list 'org-babel-load-languages (cons (intern language) t))
        (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
      (apply orig-fun args)))

  (defun irkalla/org-execute-action ()
    "Execute the appropriate action based on the given context."
    (interactive)
    (let ((context (org-element-context)))
      (pcase (org-element-type context)
        (`src-block
         ;; In a source block, call `org-babel-execute-src-block'.
         (org-babel-eval-wipe-error-buffer)
         (org-babel-execute-src-block current-prefix-arg))
        (`babel-call
         ;; In a `:+CALL:' block, call `org-babel-execute-maybe'.
         (call-interactively #'org-babel-execute-maybe))
        (`table-row
         ;; In a table or table-cell, call `org-table-next-row'.
         (call-interactively #'org-table-next-row))
        ((or `link `timestamp)
         ;; On a link or a timestamp, call `org-open-at-point'.
         (call-interactively #'org-open-at-point))
        (_
         ;; Fallback to evil standard command
         (call-interactively #'forward-line)))))

  :hook (org-babel-after-execute . org-display-inline-images)
  :custom
  (org-babel-default-header-args
   '((:async   . "yes")
     (:cache   . "no")
     (:eval    . "never-export")
     (:exports . "both")
     (:hlines  . "no")
     (:noweb   . "yes")
     (:results . "output replace")
     (:session . "none")
     (:tangle  . "no")))
  (org-export-use-babel nil)
  (org-confirm-babel-evaluate nil)
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal org-mode-map (kbd "<return>") #'irkalla/org-execute-action))
  (advice-add 'org-babel-execute-src-block :around #'demand-babel-languages))

(use-package ob-async)

(provide 'init-babel)
;;; init-babel.el ends here
