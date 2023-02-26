;;; init-corfu.el -*- lexical-binding: t -*-

(defgroup irkalla-hydra '()
  "an emacs (at-point) completion pop-up window"
  :tag "Irkalla Corfu"
  :group 'irkalla)

(use-package corfu
    :custom
    (corfu-cycle t)
    (corfu-auto t)
    ;; (corfu-separator ?\s)
    (corfu-preselect 'prompt)
    (corfu-scroll-margin 5)

    :init
    (global-corfu-mode))

(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete))

(use-package orderless
  :init
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(provide 'init-corfu)
