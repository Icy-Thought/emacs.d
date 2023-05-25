;;; essentials/init-general.el -*- lexical-binding: t -*-

(defgroup irkalla-general '()
  "More convenient key definition framework for Emacs "
  :tag "Irkalla General"
  :group 'irkalla)

(use-package general
  :demand t
  :config
  (general-evil-setup t)
  (general-override-mode)
  (general-auto-unbind-keys)

  ;; :NOTE| defining several ease-of-use bindings
  (general-create-definer irkalla/space-lead-keydef
    :states '(normal visual motion emacs insert)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")

  (general-create-definer irkalla/comma-lead-keydef
    :states '(normal visual motion emacs insert)
    :keymaps 'override
    :prefix ","
    :non-normal-prefix "M-,"))

;; Confirm that =general.el= has been configured/loaded!
(elpaca-wait)

;; :TODO| complete with more bindings..
(use-package emacs
  :elpaca nil
  :general
  (irkalla/space-lead-keydef
    ;; Buffer-related
    "b d" '(kill-this-buffer :which-key "Kill this active buffer")

    ;; Expression evaluation
    "e e" '(eval-expression  :which-key "Eval expression")
    "e b" '(eval-buffer      :which-key "Eval current Buffer")

    ;; Window-related
    "w l" '(evil-window-left :which-key "Select left window"))

  (irkalla/space-lead-keydef
    :keymaps '(visual)
    "e r" '(eval-region :which-key "Eval highlighted region")))

(provide 'init-general)
