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
  (general-evil-setup)
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

(provide 'init-general)
;;; init-general.el ends here
