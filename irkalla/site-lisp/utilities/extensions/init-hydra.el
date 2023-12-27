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
  (hydra-posframe-border-width 1)
  (hydra-posframe-parameters '((left-fringe . 25) (right-fringe . 25))))

;; :NOTE| Defining several hydra bodies

(pretty-hydra-define main-hydra
  (:title (pretty-hydra-title "──｢ Main Hydra ｣──" 'mdicon "nf-md-graph")
          :color teal :quit-key "q")
  ;; :FIXME|
  ("Buffer"    (("b" buffer-hydra/body "Benchmark"))
   "Elpaca"    (("m" elpaca-hydra/body "Elpaca"))
   "Helpful"   (("h" helpful-hydra/body "Helpful"))
   "Benchmark" (("p" benchmark-hydra/body "Benchmark"))
   "Emacs"     (("q" emacs-hydra/body "Emacs"))))

(pretty-hydra-define emacs-hydra
  (:title (pretty-hydra-title "──｢ Main: GNU Emacs ｣──" 'devicon "nf-dev-gnu")
          :color teal :quit-key "q")
  ("Emacs"
   (("r" restart-emacs "Restart")
    ("q" kill-emacs    "再见..."))))

(with-eval-after-load 'elpaca
  (pretty-hydra-define elpaca-hydra
    (:title (pretty-hydra-title "──｢ Main: Elpaca ｣──" 'pomicon "nf-pom-clean_code")
            :color teal :quit-key "q")
    ("Main"
     (("p" elpaca-manager   "Elpaca Manager")
      ("r" elpaca-rebuild   "Rebuild Package"))
     "Fetch"
     (("f" elpaca-fetch     "Specific Package")
      ("e" elpaca-fetch-all "All Packages"))
     "Update"
     (("m" elpaca-merge     "Specific Package")
      ("a" elpaca-merge-all "All Packages")))))

(pretty-hydra-define benchmark-hydra
  (:title (pretty-hydra-title "──｢ Main: Benchmark ｣──" 'faicon "nf-fa-bolt")
          :color teal :quit-key "q")
  ("Profiler"
   (("s" profiler-start "Start")
    ("k" profiler-stop  "Stop"))))

(pretty-hydra-define buffer-hydra
  (:title (pretty-hydra-title "──｢ Main: Buffer ｣──" 'octicon "nf-oct-repo_template")
          :color teal :quit-key "q")
  ("Buffer"
   (("s" scratch-buffer   "Scratch")
    ("j" next-buffer      "Next")
    ("k" previous-buffer  "Previous")
    ("d" kill-this-buffer "Exit"))
   "Evaluate"
   (("b" eval-buffer     "Buffer")
    ("e" eval-expression "Expression")
    ("f" eval-defun      "Function"))))

(pretty-hydra-define find-hydra
  (:title (pretty-hydra-title "──｢ Main: Find ｣──" 'mdicon "nf-md-file_tree")
          :color teal :quit-key "q")
  ("Find"
   (("d" find-file "File (dir)"))
   "Project"
   (("r" projectile-replace "Search & Replace"))))

(provide 'init-hydra)
;;; init-hydra.el ends here
