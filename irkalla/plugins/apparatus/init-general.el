;;; toolset/init-whichkey.el -*- lexical-binding: t -*-

(defgroup irkalla-general '()
  "More convenient key definition framework for Emacs "
  :tag "Irkalla General"
  :group 'irkalla)

(use-package general
  :config
  (general-evil-setup t)

  ;; :NOTE| define our leader-key entry
  (general-create-definer irkalla/leader-key-def
                          :keymaps '(normal insert visual emacs)
                          :prefix "SPC"
                          :global-prefix "C-SPC")

  ;; :NOTE| define our C-c entry
  (general-create-definer irkalla/ctrl-c-key-def
                          :prefix "C-c")

  ;; :TODO| complete with more bindings..
  (irkalla/leader-key-def
   "e"  '(:ignore t   :which-key "Eval expression")
   "eb" '(eval-buffer :which-key "Eval current buffer"))

  (irkalla/leader-key-def
   :keymaps '(visual)
   "er" '(eval-region :which-key "Eval highlighted region")))

(provide 'init-general)
