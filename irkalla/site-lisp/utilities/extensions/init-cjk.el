;;; init-cjk.el --- CJK Language Support -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; CJK languages are interesting and therefore having a tool which enables us to write in them would be ideal.

;;; Code:

(use-package pyim
  :general (:states 'normal :keymaps 'text-mode
                    "M-j" '(pyim-convert-string-at-point :which-key "Force convert PREV to Chinese"))
  :custom
  (pyim-default-scheme 'quanpin)
  (pyim-page-tooltip 'posframe)
  (pyim-page-length 5)
  (pyim-directory (no-littering-expand-var-file-name "pyim/"))
  (pyim-dcache-directory (pyim-directory "dcache/")))

(use-package pyim-basedict
  :requires (pyim)
  :hook (pyim-mode . pyim-basedict-enable))

(use-package youdao-dictionary
  :commands (youdao-dictionary-search-at-point-posframe)
  :general (:states 'normal :keymaps text-mode-map
                    "C-M-y" '(youdao-dictionary-search-at-point-posframe :which-key "Dict lookup on point")))

(provide 'init-cjk)
;;; init-cjk.el ends here
