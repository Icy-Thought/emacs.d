;;; init-meow.el --- Meow: Modular Bindings in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; A quicker "Emacs Native" experience for the Emacs user and could be said to be more reasonable than Evil.

;;; Code:

(defun meow-qwerty-setup ()
  "Simple bindings intended to make our Meow workflow more fluid."
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
        meow-use-cursor-position-hack t)

  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . keyboard-quit))

  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")

   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)

   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))

  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)

   '("~" . upcase-char)
   '("-" . negative-argument)
   '("=" . indent-region)
   '(":" . recenter-top-bottom)
   '(";" . meow-reverse)
   '("." . repeat)
   '("<" . meow-inner-of-thing)
   '(">" . meow-bounds-of-thing)
   '("(" . backward-sexp)
   '(")" . forward-sexp)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)

   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("P" . meow-yank-pop)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . undo)
   '("U" . undo-redo)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("<escape>" . keyboard-quit)))

(defun meow-colemak-dh-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak)
  (meow-motion-overwrite-define-key
   '("e" . meow-prev)
   '("<escape>" . ignore))

  (meow-leader-define-key
   '("?" . meow-cheatsheet)
   '("e" . "H-e")
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument))

  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("1" . meow-expand-1)
   '("2" . meow-expand-2)
   '("3" . meow-expand-3)
   '("4" . meow-expand-4)
   '("5" . meow-expand-5)
   '("6" . meow-expand-6)
   '("7" . meow-expand-7)
   '("8" . meow-expand-8)
   '("9" . meow-expand-9)

   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("/" . meow-visit)
   '("'" . repeat)
   '("<escape>" . ignore)

   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("e" . meow-prev)
   '("E" . meow-prev-expand)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-right)
   '("I" . meow-right-expand)
   '("j" . meow-join)
   '("k" . meow-kill)
   '("l" . meow-line)
   '("L" . meow-goto-line)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("s" . meow-insert)
   '("S" . meow-open-above)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-search)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-delete)
   '("X" . meow-backward-delete)
   '("y" . meow-save)
   '("z" . meow-pop-selection)))

(use-package meow
  :demand t
  :hook (elpaca-after-init . meow-global-mode)
  :custom-face
  (meow-beacon-indicator ((t (:inherit telephone-line-evil-emacs))))
  (meow-insert-indicator ((t (:inherit telephone-line-evil-insert))))
  (meow-motion-indicator ((t (:inherit telephone-line-evil-motion))))
  (meow-normal-indicator ((t (:inherit telephone-line-evil-motion))))
  (meow-keypad-indicator ((t (:inherit telephone-line-evil-operator))))
  :custom
  (meow-keypad-self-insert-undefined nil)
  (meow-keypad-ctrl-meta-prefix nil)
  (meow-keypad-literal-prefix ?\s)
  (meow-keypad-meta-prefix ?m)
  (meow-keypad-start-keys '((?c . ?c) (?u . ?u) (?x . ?x)))
  (meow-mode-state-list '((vterm-mode . insert)))

  (meow-expand-hint-remove-delay 1.5)
  (meow-esc-delay 0.001)
  (meow-use-clipboard t)
  (meow-expand-exclude-mode-list nil)
  :config
  (meow-qwerty-setup)
  (meow-setup-indicator)
  (meow-setup-line-number)

  ;; :NOTE| meow-things related bindings
  (meow-thing-register 'tex-round    '(pair ("\\left(") ("\\right)"))     '(pair ("\\left(") ("\\right)")))
  (meow-thing-register 'tex-square   '(pair ("\\left[") ("\\right]"))     '(pair ("\\left[") ("\\right]")))
  (meow-thing-register 'tex-curly    '(pair ("\\left\\{") ("\\right\\}")) '(pair ("\\left\\{") ("\\right\\}")))

  (meow-thing-register 'quote-gqq    '(pair ("„") ("“"))                  '(pair ("„") ("“")))
  (meow-thing-register 'quote-gq     '(pair ("‚") ("‘"))                  '(pair ("‚") ("‘")))
  (meow-thing-register 'quote-eqq    '(pair ("“") ("”"))                  '(pair ("“") ("”")))
  (meow-thing-register 'quote-eq     '(pair ("‘") ("’"))                  '(pair ("‘") ("’")))

  ;; Later we inform Meow about our new changes
  (setopt meow-char-thing-table
          '((?\( . round)     (?\) . round)
            (?\[ . square)    (?\] . square)
            (?\{ . curly)     (?\} . curly)
            (?r  . tex-round) (?s  . tex-square) (?c . tex-curly)
            (?„  . quote-gqq) (?‚  . quote-gq)
            (?\“ . quote-eqq) (?\‘ . quote-eq)
            (?s  . symbol)    (?.  . sentence)   (?b . buffer)
            (?p  . paragraph) (?l  . line)       (?\" . string))))

(provide 'init-meow)
;;; init-meow.el ends here
