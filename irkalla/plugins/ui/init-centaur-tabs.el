;;; ui/init-centaur-tabs.el -*- lexical-binding: t -*-

(defgroup irkalla-centaur-tabs '()
  "a rather modern looking tabs plugin"
  :tag "Irkalla Centaur-Tabs"
  :group 'irkalla)

(use-package centaur-tabs
  :demand
  :init
  (setq centaur-tabs-enable-key-bindings t)
  :config
  (setq centaur-tabs-height 32
	centaur-tabs-set-icons t
	centaur-tabs-show-new-tab-button t
	centaur-tabs-set-modified-marker t
	centaur-tabs-show-navigation-buttons t
	centaur-tabs-set-bar 'under
	centaur-tabs-show-count nil
	x-underline-at-descent-line t
	centaur-tabs-left-edge-margin nil)

  (centaur-tabs-change-fonts (face-attribute 'default :font) 110)
  (centaur-tabs-headline-match)
  ;; (setq centaur-tabs-adjust-buffer-order t)
  (centaur-tabs-mode t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

(provide 'init-centaur-tabs)
