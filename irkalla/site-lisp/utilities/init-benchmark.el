;;; init-benchmark.el --- Benchmark Emacs Performance -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Emacs is not slow, we just accidentally made it this way due to our lack of understanding of a proper config.

;;; Code:

(use-package esup
  :defer t
  :custom (esup-depth 0))

(provide 'init-benchmark)
;;; init-benchmark.el ends here
