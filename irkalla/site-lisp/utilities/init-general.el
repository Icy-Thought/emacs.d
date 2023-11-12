;;; init-general.el --- General: Sane Binding Management -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Using ~:bind~ is often not enough (evil), thus we ought to introduce something more general to the party.

;;; Code:

(use-package general
  :demand t
  :config
  (if (featurep 'evil) (general-evil-setup))
  (general-override-mode)
  (general-auto-unbind-keys)

  ;; :NOTE| defining several ease-of-use bindings
  (general-create-definer irkalla/space-lead-keydef
    :keymaps 'override
    :states '(emacs insert motion normal visual)
    :prefix "SPC"
    :global-prefix "M-SPC")

  (general-create-definer irkalla/comma-lead-keydef
    :keymaps 'override
    :states '(emacs insert motion normal visual)
    :prefix ","
    :non-normal-prefix "M-,"))

;; :NOTE| Necessary for :general to work with use-package!
(elpaca-wait)

;; :NOTE| Defining several bindings for General.el
(use-package emacs
  :elpaca nil
  :general
  (general-def "<escape>" 'keyboard-escape-quit)

  (irkalla/space-lead-keydef
    "b"     '(:ignore t          :which-key "Buffer")
    "b d"   '(kill-this-buffer   :which-key "Exit active buffer")
    "b n"   '(next-buffer        :which-key "Switch -> next buffer")
    "b p"   '(previous-buffer    :which-key "Switch -> prev. buffer")
    "b s"   '(scratch-buffer     :which-key "Switch -> curr. scratch-buf")

    "f"     '(:ignore t          :which-key "Find")
    "f RET" '(find-file          :which-key "Files in directory")

    "e"     '(:ignore t          :which-key "Evaluation")
    "e e"   '(eval-expression    :which-key "Evaluate input")
    "e b"   '(eval-buffer        :which-key "Evaluate buffer")

    "p"     '(:ignore t          :which-key "Projects")
    "p r"   '(projectile-replace :which-key "Search & replace str")

    "q"     '(:ignore t          :which-key "Manage Emacs sessions")
    "q r"   '(restart-emacs      :which-key "Restart Emacs")
    "q q"   '(kill-emacs         :which-key "Quit Emacs..."))

  (irkalla/space-lead-keydef
    :states 'visual
    "e r"   '(eval-region        :which-key "Eval highlighted"))

  (irkalla/comma-lead-keydef
    "b"   '(:ignore t               :which-key "Buffer")

    "g"   '(:ignore t               :which-key "Profiling")
    "g s" '(profiler-start          :which-key "Start Profiling")
    "g k" '(profiler-stop           :which-key "Stop Profiling")

    "p"   '(:ignore t               :which-key "Package Manager")
    "p p" '(elpaca-manager          :which-key "Open Elpaca")
    "p f" '(elpaca-fetch-all        :which-key "Fetch package commits")
    "p u" '(elpaca-update-all       :which-key "Update all packages")))

(provide 'init-general)
;;; init-general.el ends here
