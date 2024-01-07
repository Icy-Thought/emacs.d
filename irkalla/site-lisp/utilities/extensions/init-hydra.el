;;; init-hydra.el --- Hydra: Sane Binding Management -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Using ~:bind~ is often not enough (evil), thus we ought to introduce something more extensible to the party.

;;; Code:

(use-package pretty-hydra
  :demand t
  :config
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon to our pretty-hydra title(s)."
    (let ((face (or face `(:inherit hydra-face-pink :height 1.2 :slant italic)))
          (height (or height 1.2))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (display-graphic-p) icon-type icon-name)
         (let ((f (intern (format "nerd-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              "  "))))
       (propertize title 'face face)))))

(elpaca-wait)

(use-package hydra-posframe
  :elpaca (:host github :repo "Ladicle/hydra-posframe")
  :hook (elpaca-after-init . hydra-posframe-mode)
  :custom
  (hydra-posframe-border-width 2)
  (hydra-posframe-parameters '((left-fringe . 25) (right-fringe . 25))))

;; :NOTE| Defining several hydra bodies

(pretty-hydra-define main-hydra
  (:title (pretty-hydra-title "──｢ Phylum Cnidaria ｣──" 'mdicon "nf-md-graph")
          :color teal :quit-key "q")
  ("Main"
   (("o" launcher-hydra/body "Launcher")
    ("p" elpaca-hydra/body "Elpaca"))
   "Control"
   (("b" buffer-hydra/body "Buffer"))))

(with-eval-after-load 'evil
  (evil-global-set-key 'normal (kbd "SPC") 'main-hydra/body))

(with-eval-after-load 'meow
  (meow-normal-define-key '("S-SPC" . main-hydra/body)))

;; :NOTE| My custom hydras are located below.

(pretty-hydra-define buffer-hydra
  (:title (pretty-hydra-title "──｢ Main: Buffer(s) ｣──" 'octicon "nf-oct-repo_template")
          :color teal :quit-key "q")
  ("Buffer"
   (("s" scratch-buffer   "Scratch")
    ("j" next-buffer      "Next")
    ("k" previous-buffer  "Previous"))))

(pretty-hydra-define launcher-hydra
  (:title (pretty-hydra-title "──｢ Main: Launcher(s) ｣──" 'codicon "nf-cod-rocket")
          :color teal :quit-key "q")
  ("EWW Browse"
   (("w" (eww-browse-url "https://en.wikipedia.org") "Wikipedia"))))

(with-eval-after-load 'elpaca
  (pretty-hydra-define elpaca-hydra
    (:title (pretty-hydra-title "──｢ Main: Elpaca ｣──" 'pomicon "nf-pom-clean_code")
            :color teal :quit-key "q")
    ("Main"
     (("p" elpaca-manager   "Elpaca manager")
      ("r" elpaca-rebuild   "Rebuild package")
      ("i" elpaca-info      "Package info"))
     "Fetch"
     (("f" elpaca-fetch     "Specific package")
      ("e" elpaca-fetch-all "All packages"))
     "Update"
     (("m" elpaca-merge     "Specific package")
      ("a" elpaca-merge-all "All packages")))))

(provide 'init-hydra)
;;; init-hydra.el ends here
