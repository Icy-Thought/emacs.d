;;; my-content.el --- Rules For Displaying Content -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-feature emacs
  :custom
  (fill-column 120)
  (truncate-lines nil))

(use-feature visual-wrap
  :if (>= emacs-major-version 30)
  :hook (((prog-mode conf-mode) . visual-wrap-prefix-mode)
         (visual-wrap-prefix-mode . visual-line-mode)))

(use-package visual-fill-column
  :commands (visual-fill-column-mode)
  :hook ((text-mode . visual-fill-column-mode)
         (visual-fill-column-mode . visual-line-mode))
  :custom (visual-fill-column-center-text t))

;; :NOTE| Styled whitespaces & automated cleanups

(use-feature whitespace
  :hook (before-save . whitespace-cleanup)
  :custom
  (whitespace-style
   '(face tab space newline
          space-before-tab space-after-tab
          indentation trailing))
  (whitespace-display-mappings
   '((tab-mark ?\t [?» ?\t])
     (space-mark ?\  [?·] [?.])
     (newline-mark ?\n [?¬ ?\n]))))

;; :NOTE| Replace $SYMB with cleaner alternative
;; :WARN| A font which supports ligatures is required!

(use-package ligature
  :config
  (global-ligature-mode t)
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (ligature-set-ligatures
   'prog-mode
   '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
     ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
     "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
     "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
     "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
     "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
     "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
     "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
     ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
     "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
     "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
     "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
     "\\\\" "://")))

(provide 'my-content)
