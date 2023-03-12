;;; completion/init-corfu.el -*- lexical-binding: t -*-

(defgroup irkalla-corfu '()
  "an emacs (at-point) completion pop-up window"
  :tag "Irkalla Corfu"
  :group 'irkalla)

(use-package corfu
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :init (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 0)
  (corfu-cycle t)
  (corfu-on-exact-match 'insert)
  (corfu-preselect 'prompt)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match t)
  (corfu-scroll-margin 5)
  (corfu-separator ?\s))

(use-package corfu-popupinfo
  :ensure nil
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom (corfu-popupinfo-delay '(0.2 . t)))


(use-package kind-icon
  :after corfu
  :custom (kind-icon-default-face 'corfu-default)
  :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(provide 'init-corfu)
