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
  :bind (:map corfu-map
	      ("C-n" . corfu-next)
	      ("TAB" . corfu-next)
	      ([tab] . corfu-next)
	      ("C-p" . corfu-previous)
	      ("S-TAB" . corfu-previous)
	      ([backtab] . corfu-previous)
	      ("RET" . corfu-complete-and-quit)
	      ("<return>" . corfu-complete-and-quit)
	      ("C-g" . corfu-quit)
	      ("C-q" . corfu-quick-insert)
	      ("S-SPC" . corfu-insert-separator)
	      ([remap completion-at-point] . corfu-complete)
	      ("M-d" . corfu-popupinfo-toggle)
	      ("M-p" . corfu-popupinfo-scroll-down)
	      ("M-n" . corfu-popupinfo-scroll-up))
  :custom
  (corfu-cycle nil)
  (corfu-auto t)
  (corfu-count 9)
  (corfu-on-exact-match 'quit)
  (corfu-preselect-first t)
  (corfu-quit-at-boundary 'separator)
  (corfu-auto-delay 0.0)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match t)
  (corfu-scroll-margin 5)
  (corfu-max-width 100)
  (corfu-min-width 42)
  (corfu-popupinfo-max-height 20)
  (corfu-popupinfo-max-width 85))

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
