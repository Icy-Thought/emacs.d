;;; editor/init-meow.el -*- lexical-binding: t -*-

(defgroup irkalla-meow '()
  "A lightweight modular editing system."
  :tag "Irkalla Meow"
  :group 'irkalla)

(defun meow-setup-qwerty ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

  ;; Where the def. of our Meow happens:
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))

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

   ;; Quicker navigation's
   '("df" . fd-dired)
   '("r" . consult-recent-file)
   '("f" . +vertico/find-file-in)
   '("F" . consult-find)
   '("da" . consult-rg)
   '("SPC" . meow-M-x)
   '("ti" . insert-current-date-time-inactive)
   '("ta" . insert-current-date-time-active)
   '("tc" . insert-current-date-time)
   '("bs" . bookmark-set)
   '("bm" . bookmark-bmenu-list)
   '("bb" . switch-to-buffer)
   '("bw" . +vertico/switch-workspace-buffer)
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
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
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
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))


(use-package meow
  :demand t
  :init (meow-global-mode 1)
  :custom (meow-esc-delay 0.001)
  :config
  (setq meow-mode-state-list
        '((fundamental-mode . normal)
          (text-mode . normal)
          (prog-mode . normal)
          (conf-mode . normal)
          (eaf-mode . insert)))

  (meow-setup-qwerty)
  (meow-setup-indicator)
  (meow-setup-line-number)
  (add-hook 'meow-normal-mode-hook #'corfu-quit)
  (add-to-list 'meow-update-cursor-functions-alist (cons 'meow--cursor-null-p (lambda ())))) ; Treemacs

(provide 'init-meow)
