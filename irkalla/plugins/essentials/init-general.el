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
    :keymaps 'override
    :states '(emacs insert motion normal visual)
    :prefix "SPC"
    :global-prefix "M-SPC")

  (general-create-definer irkalla/comma-lead-keydef
    :keymaps 'override
    :states '(emacs insert motion normal visual)
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
    "b"   '(:ignore t        :which-key "Buffer Management")
    "b d" '(kill-this-buffer :which-key "Kill active buffer")

    ;; Expression evaluation
    "e"   '(:ignore t        :which-key "Evaluation")
    "e e" '(eval-expression  :which-key "Evaluate input expression")
    "e b" '(eval-buffer      :which-key "Evaluate buffer"))

  (irkalla/space-lead-keydef
    :states '(visual)
    "e r" '(eval-region :which-key "Eval highlighted region")))

(provide 'init-general)
