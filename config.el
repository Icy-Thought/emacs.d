;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Icy-Thought"
      user-mail-address "icy-thought@pm.me")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
(setq doom-theme 'doom-tokyo-night)

;; Invert color swapping (solaire-mode)
(with-eval-after-load 'solaire-mode
  (add-to-list 'solaire-mode-themes-to-face-swap 'doom-tokyo-night))

;; We don't always want the transparency active, do we?
(defun toggle-window-transparency ()
  "Disable window transparency on demand."
  (interactive)
  (let ((alpha-transparency 85))
    (pcase (frame-parameter nil 'alpha-background)
      (alpha-transparency (set-frame-parameter nil 'alpha-background 100))
      (t (set-frame-parameter nil 'alpha-background alpha-transparency)))))

;; Making our beloved font more aesthetically pleasing!
(setq doom-font (font-spec :family "VictorMono Nerd Font" :size 12.0 :weight 'semi-bold)
      doom-big-font (font-spec :family "VictorMono Nerd Font" :size 15.0 :weight 'semi-bold)
      doom-variable-pitch-font (font-spec :family "VictorMono Nerd Font" :size 12.0 :weight 'semi-bold)
      doom-unicode-font (font-spec :family "VictorMono Nerd Font" :size 12 :weight 'semi-bold))

;; Throwing in a hint of italic for our font:
(custom-set-faces!
  '(font-lock-builtin-face :slant italic)
  '(font-lock-comment-face :slant italic)
  '(font-lock-function-name-face :weight bold :slane italic)
  '(font-lock-keyword-face :slant italic))

;; (Org-Mode): Quotes shall be italic!
(setq org-fontify-quote-and-verse-blocks t)

;; (Org-Mode): Different headline sizes!
(custom-set-faces!
  '(org-document-title :height 1.2)
  '(outline-1 :weight extra-bold :height 1.25)
  '(outline-2 :weight bold :height 1.15)
  '(outline-3 :weight bold :height 1.12)
  '(outline-4 :weight semi-bold :height 1.09)
  '(outline-5 :weight semi-bold :height 1.06)
  '(outline-6 :weight semi-bold :height 1.03)
  '(outline-8 :weight semi-bold)
  '(outline-9 :weight semi-bold))

;;(Markdown): Different headline sizes!
(custom-set-faces!
  '(markdown-header-face-1 :height 1.25 :weight extra-bold :inherit markdown-header-face)
  '(markdown-header-face-2 :height 1.15 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-3 :height 1.08 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-4 :height 1.00 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-5 :height 0.90 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-6 :height 0.75 :weight extra-bold :inherit markdown-header-face))

;; Saner defaults:
(setq-default
 history-length 1000                              ; More = history -> better retention!
 prescient-history-length 1000                    ; For how long we retain that info.
 delete-by-moving-to-trash t                      ; Delete files to trash
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

;; Options to enhance our workflow:
(setq
 auto-save-default t                            ; We don't like to lose unsaved work, do wo?
 display-line-numbers-type 'relative            ; Relative number-line
 evil-want-fine-undo t                          ; More granular undo-tree, rahter than one large blob:
 truncate-string-ellipsis "↴"                   ; End lines with unicode rather than "..."
 scroll-margin 2                                ; Quicker scrolling!
 which-key-idle-delay 0.5                       ; Faster Which-Key prompt!
 org-directory "~/org/")                        ; Where our `Org' directory resides.

;; Enable mode-line timer
(display-time-mode 1)                             ; Enable time in the mode-line

;; Enable mode-line battery-level if AC == True
(unless (string-match-p "^Power N/A" (battery))
  (display-battery-mode 1))

;; Iterate through CamelCase:
(global-subword-mode 1)

;; Quicker LSP Response
(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2)
  (setq company-show-quick-access t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.

;; Vim-like splits:
(setq evil-vsplit-window-right t
      evil-split-window-below t)

;; Switch to buffer after split
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))

;; Better window navigation:
(map! :map evil-window-map
      ;; Navigation
      "<left>"     #'evil-window-left
      "<down>"     #'evil-window-down
      "<up>"       #'evil-window-up
      "<right>"    #'evil-window-right

      ;; Swapping windows
      "C-<left>"       #'+evil/window-move-left
      "C-<down>"       #'+evil/window-move-down
      "C-<up>"         #'+evil/window-move-up
      "C-<right>"      #'+evil/window-move-right)

;; Mouse-related mappings:
(map! :n [mouse-8] #'scroll-up-command
      :n [mouse-9] #'scroll-down-command)

;; Reconfigure a package? => wrap in `after!' block!
;; WARN: Doom's defaults may override your settings if done otherwise!
;;
;; The exceptions to this rule:
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Additional functions/macros:
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; More info about ^ -> hover -> ~Shift+k~!

;; Auto-Download emojies:
(after! emojify
  (setq emojify-download-emojis-p t))

;; (PDF-Tools): default view mode + new theme:
(after! pdf-tools
  (setq-default pdf-view-display-size 'fit-width)
  (add-hook! 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode))

;; Language Specific Settings:
;;
;; Tell Emacs about our prefered haskell formatter:
(after! haskell-mode
  (set-formatter! 'stylish-haskell "stylish-haskell" :modes '(haskell-mode)))

;; Prefered: Alejandra formatter!
(after! nix-mode
  ;; TODO: format on save
  (set-formatter! 'alejandra "alejandra" :modes '(nix-mode)))

;; How our lua code shall be formatted:
(after! lua-mode
  (set-formatter! 'stylua "stylua -" :modes '(lua-mode)))
