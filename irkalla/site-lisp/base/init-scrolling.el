;;; init-scrolling.el --- Re-defining Scrolling Behaviour -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Default scrolling & scrolling bar needs a bit of spice, thus we have srpinkled a little of it until we discover the missing bits.

;;; Code:

(use-package emacs
  :elpaca nil
  :hook (elpaca-after-init . pixel-scroll-precision-mode)
  :custom
  (auto-window-vscroll nil)
  (fast-but-imprecise-scrolling nil)
  (hscroll-margin 1)
  (hscroll-step 1)
  (lazy-lock-defer-on-scrolling t)
  (mouse-wheel-progressive-speed nil)
  (mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (scroll-conservatively 101)
  (scroll-down-aggressively 0.01)
  (scroll-margin 3)
  (scroll-preserve-screen-position t)
  (scroll-step 1)
  (scroll-up-aggressively 0.01))

(provide 'init-scrolling)
;;; init-scrolling.el ends here
