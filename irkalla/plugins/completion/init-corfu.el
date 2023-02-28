;;; completion/init-corfu.el -*- lexical-binding: t -*-

(defgroup irkalla-hydra '()
  "an emacs (at-point) completion pop-up window"
  :tag "Irkalla Corfu"
  :group 'irkalla)

(use-package corfu
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode +1)
  (add-hook 'org-mode-hook (lambda () (corfu-mode -1)))
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 0)
  (corfu-count 9)
  (corfu-cycle t)
  (corfu-max-width 100)
  (corfu-min-width 42)
  (corfu-on-exact-match 'insert)
  (corfu-popupinfo-max-height 20)
  (corfu-popupinfo-max-width 85)
  (corfu-preselect 'prompt)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match t)
  (corfu-scroll-margin 5))

(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete))

(use-package prescient
  :custom
  (prescient-sort-full-matches-first t)
  (prescient-sort-length-enable nil)
  (prescient-history-length 1000)
  (prescient-save-file
   (expand-file-name "prescient-save.el"
		     no-littering-var-directory))
  :config
  (prescient-persist-mode +1))

(use-package corfu-prescient
  :init
  (setq corfu-prescient-override-sorting t
	corfu-prescient-enable-filtering nil)
  :config
  (corfu-prescient-mode +1))

(provide 'init-corfu)
