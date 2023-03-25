;;; langserv/init-babel.el -*- lexical-binding: t -*-

;; Asynchronous org-babel evaluation!
(use-package ob-async
  :after org)

;; External Packages
;; (use-package ob-mermaid)

;; Calling org-babel language on demand
(defadvice org-babel-execute-src-block (around load-language nil activate)
  "Load language if needed"
  (let ((language (org-element-property :language (org-element-at-point))))
    (unless (cdr (assoc (intern language) org-babel-load-languages))
      (add-to-list 'org-babel-load-languages (cons (intern language) t))
      (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
    ad-do-it))

(provide 'init-babel)
