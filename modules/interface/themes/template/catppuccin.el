;;; package: --- Catppuccin: template for our theme variants -*- lexical-binding: t;; -*-

;;; Commentary:
;;; [1]: https://github.com/catppuccin/catppuccin

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'autothemer)

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(defmacro catppuccin-deftheme (name description palette &rest body)
  `(autothemer-deftheme
    ,name
    ,description
    ,palette
    (;; Built-in Emacs faces
     (default                                           (:background base :foreground text))
     (border                                            (:background base :foreground base))
     (bookmark-face                                     (:background base :foreground red))
     (button                                            (:foreground green))
     (child-frame                                       (:background base :foreground mantle))
     (child-frame-border                                (:background base :foreground mantle))
     (cursor                                            (:background flamingo :foreground base))
     (error                                             (:foreground red))
     (fringe                                            (:foreground mantle))
     (glyph-face                                        (:background red))
     (glyphless-char                                    (:foreground overlay2))
     (header-line                                       (:background base))
     (highlight                                         (:background lavender :foreground crust))
     (hl-line                                           (:background surface0))
     (homoglyph                                         (:foreground teal))
     (internal-border                                   (:background base :foreground base))
     (line-number                                       (:background base :foreground surface1))
     (line-number-current-line                          (:background flamingo :foreground base :bold t))
     (link                                              (:foreground maroon :underline t))
     (custom-link                                       (:foreground maroon :underline t))
     (lv-separator                                      (:background base :foreground overlay2))
     (match                                             (:background yellow :foreground crust))
     (menu                                              (:background base :foreground rosewater))
     (fill-column-indicator                             (:foreground surface0))
     (mode-line                                         (:background mantle))
     (mode-line-inactive                                (:background base :foreground subtext1 :bold nil))
     (mode-line-active                                  (:background mantle :foreground subtext0 :bold t))
     (mode-line-highlight                               (:foreground flamingo))
     (mode-line-buffer-id                               (:foreground pink :bold t))
     (numbers                                           (:background green :foreground base))
     (region                                            (:inherit 'highlight :italic t))
     (separator-line                                    (:background base))
     (shadow                                            (:background crust))
     (success                                           (:foreground green))
     (vertical-border                                   (:background nil :foreground surface0))
     (warning                                           (:foreground yellow))
     (window-border                                     (:foreground peach))
     (window-divider                                    (:foreground surface0))

     ;; https://www.emacswiki.org/emacs/WhiteSpace
     (whitespace-tab                                    (:foreground overlay1))
     (whitespace-space                                  (:foreground overlay1))
     (whitespace-space-before-tab                       (:inherit 'whitespace-space))
     (whitespace-space-after-tab                        (:inherit 'whitespace-space))
     (whitespace-empty                                  (:foreground yellow))
     (whitespace-line                                   (:foreground pink))
     (whitespace-newline                                (:foreground overlay1))
     (whitespace-missing-newline-at-eof                 (:foreground yellow))
     (whitespace-indentation                            (:foreground overlay1))
     (whitespace-big-indent                             (:foreground maroon))
     (whitespace-trailing                               (:foreground maroon))

     (trailing-whitespace                               (:background overlay1))

     ;; https://www.emacswiki.org/emacs/FontLockMode
     (elisp-shorthand-font-lock-face                    (:foreground peach))
     (font-lock-builtin-face                            (:foreground peach))
     (font-lock-comment-delimiter-face                  (:foreground overlay2 :italic t))
     (font-lock-comment-face                            (:foreground overlay1 :italic t))
     (font-lock-constant-face                           (:foreground peach))
     (font-lock-constant-face                           (:foreground lavender))
     (font-lock-doc-face                                (:foreground overlay2))
     (font-lock-doc-markup-face                         (:foreground overlay2))
     (font-lock-function-name-face                      (:foreground blue))
     (font-lock-keyword-face                            (:foreground mauve :weight 'semi-bold))
     (font-lock-negation-char-face                      (:foreground red))
     (font-lock-preprocessor-face                       (:foreground overlay2))
     (font-lock-reference-face                          (:foreground sky))
     (font-lock-regexp-grouping-backslash               (:foreground peach))
     (font-lock-string-face                             (:foreground green :italic t))
     (font-lock-type-face                               (:foreground yellow))
     (font-lock-variable-name-face                      (:foreground text))
     (font-lock-warning-face                            (:inherit 'warning))

     ;; https://www.emacswiki.org/emacs/MiniBuffer#minibuffer
     (minibuffer-prompt-end                             (:background crust :foreground red))
     (minibuffer-prompt                                 (:background crust :foreground mauve))

     (epa-mark                                          (:foreground pink))
     (info-xref                                         (:foreground yellow))

     ;; https://www.emacswiki.org/emacs/DiredMode
     (dired-mark                                        (:foreground pink))
     (dired-ignored                                     (:background yellow))

     ;; https://github.com/purcell/diredfl
     (diredfl-compressed-file-name                      (:foreground green))
     (diredfl-compressed-file-suffix                    (:foreground green))
     (diredfl-deletion                                  (:background red :foreground crust))
     (diredfl-dir-name                                  (:foreground mauve))
     (diredfl-dir-heading                               (:foreground flamingo :height 1.15 :bold t :italic t))
     (diredfl-file-name                                 (:foreground pink))
     (diredfl-file-suffix                               (:foreground lavender))
     (diredfl-flag-mark                                 (:background blue :foreground crust))
     (diredfl-link-prev                                 (:foreground blue))
     (diredfl-number                                    (:foreground peach))

     (diredfl-no-priv                                   (:foreground surface0))
     (diredfl-dir-priv                                  (:foreground blue))
     (diredfl-read-priv                                 (:foreground green))
     (diredfl-write-priv                                (:foreground yellow))
     (diredfl-exec-priv                                 (:foreground red))

     ;; https://github.com/alexluigit/dirvish
     (dirvish-git-commit-message-face                   (:background yellow :foreground crust))
     (dirvish-hl-line                                   (:background flamingo :foreground crust))
     (dirvish-subtree-guide                             (:foreground surface0))
     (dirvish-subtree-state                             (:foreground surface0))
     (dirvish-vc-needs-merge-face                       (:background red :foreground crust))

     ;; https://github.com/jaypei/emacs-neotree
     (neo-dir-link-face                                 (:foreground blue :weight 'bold))
     (neo-expand-btn-face                               (:foreground peach))
     (neo-file-link-face                                (:foreground text))
     (neo-root-dir-face                                 (:foreground mauve :weight 'bold))

     (neo-icon-dir-face                                 (:foreground teal))
     (neo-icon-file-face                                (:foreground text))
     (neo-icon-tag-directory-face                       (:foreground sapphire))
     (neo-icon-tag-file-face                            (:foreground teal))
     (neo-icon-tag-text-face                            (:foreground overlay1))

     (neo-vc-added-face                                 (:foreground green))
     (neo-vc-conflict-face                              (:foreground red :weight 'bold))
     (neo-vc-default-face                               (:foreground text))
     (neo-vc-edited-face                                (:foreground yellow))
     (neo-vc-ignored-face                               (:foreground surface2))
     (neo-vc-missing-face                               (:foreground maroon :weight 'bold))
     (neo-vc-needs-merge-face                           (:foreground pink :weight 'bold))
     (neo-vc-removed-face                               (:foreground red :weight 'bold))
     (neo-vc-unlocked-changes-face                      (:foreground peach))
     (neo-vc-unregistered-face                          (:foreground overlay2))
     (neo-vc-up-to-date-face                            (:foreground text))

     ;; https://github.com/nex3/perspective-el
     (persp-selected-face                               (:foreground blue :weight 'bold :italic t))

     ;; https://www.emacswiki.org/emacs/Iedit
     (iedit-occurrence                                  (:background blue :foreground crust))
     (iedit-read-only-occurrence                        (:background yellow :foreground crust))

     ;; https://github.com/seagle0128/doom-modeline
     (doom-modeline-battery-charging                    (:foreground overlay2))
     (doom-modeline-battery-critical                    (:inherit 'error))
     (doom-modeline-battery-error                       (:inherit 'error))
     (doom-modeline-battery-full                        (:foreground green))
     (doom-modeline-battery-normal                      (:foreground pink))
     (doom-modeline-battery-warning                     (:inherit 'warning))

     (doom-modeline-evil-emacs-state                    (:foreground blue))
     (doom-modeline-evil-insert-state                   (:foreground mauve))
     (doom-modeline-evil-motion-state                   (:foreground teal))
     (doom-modeline-evil-normal-state                   (:foreground pink))
     (doom-modeline-evil-operator-state                 (:foreground blue))
     (doom-modeline-evil-replace-state                  (:foreground yellow))
     (doom-modeline-evil-visual-state                   (:foreground lavender))

     (doom-modeline-bar                                 (:background base))
     (doom-modeline-buffer-file                         (:foreground pink :bold t))
     (doom-modeline-buffer-major-mode                   (:foreground peach :italic t))
     (doom-modeline-buffer-modified                     (:foreground yellow :italic t :bold t))
     (doom-modeline-buffer-path                         (:foreground overlay2))
     (doom-modeline-error                               (:background red))
     (doom-modeline-info                                (:foreground green))
     (doom-modeline-panel                               (:inherit 'bold :background flamingo :foreground mantle))
     (doom-modeline-project-dir                         (:foreground peach))
     (doom-themes-visual-bell                           (:background red))

     ;; https://github.com/TheBB/spaceline
     (powerline-active0                                 (:background crust))
     (powerline-active1                                 (:background mantle))
     (powerline-active2                                 (:background base))

     (powerline-inactive0                               (:background crust))
     (powerline-inactive1                               (:background mantle))
     (powerline-inactive2                               (:background base))

     (spaceline-highlight-face                          (:background blue :foreground mantle))
     (spaceline-modified                                (:background yellow))
     (spaceline-unmodified                              (:background pink))
     (spaceline-python-venv                             (:background rosewater))
     (spaceline-read-only                               (:background red))

     (spaceline-evil-emacs                              (:background pink     :foreground mantle))
     (spaceline-evil-insert                             (:background green    :foreground mantle))
     (spaceline-evil-motion                             (:background teal     :foreground mantle))
     (spaceline-evil-normal                             (:background blue     :foreground mantle))
     (spaceline-evil-operator                           (:background mauve    :foreground mantle))
     (spaceline-evil-replace                            (:background yellow   :foreground mantle))
     (spaceline-evil-visual                             (:background lavender :foreground mantle))

     ;; https://github.com/dbordak/telephone-line
     (telephone-line-accent-active                      (:background base :foreground pink :weight 'bold :italic t))
     (telephone-line-accent-inactive                    (:background base :foreground surface0))
     (telephone-line-projectile                         (:foreground peach :weight 'bold :italic t))

     (telephone-line-evil-normal                        (:background pink :foreground mantle :bold t))
     (telephone-line-evil-visual                        (:background lavender :foreground mantle :bold t))
     (telephone-line-evil-motion                        (:background teal :foreground mantle :bold t))
     (telephone-line-evil-insert                        (:background mauve :foreground mantle :bold t))

     ;; https://www.emacswiki.org/emacs/eww
     (eww-form-file                                     (:background mantle :foreground maroon))
     (eww-form-text                                     (:background mantle :foreground text))
     (eww-form-select                                   (:background mantle :foreground pink))
     (eww-form-submit                                   (:background mantle :foreground yellow))
     (eww-form-checkbox                                 (:background mantle :foreground teal))
     (eww-form-textarea                                 (:background mantle :foreground text))
     (eww-valid-certificate                             (:foreground blue :italic t))
     (eww-invalid-certificate                           (:foreground red  :italic t))

     ;; https://www.gnu.org/software/emacs/manual/html_node/eww/Advanced.html
     (shr-h1                                            (:inherit 'org-document-title))
     (shr-h2                                            (:inherit 'org-level-1))
     (shr-h3                                            (:inherit 'org-level-2))
     (shr-h4                                            (:inherit 'org-level-3))
     (shr-h5                                            (:inherit 'org-level-4))
     (shr-h6                                            (:inherit 'org-level-5))
     (shr-text                                          (:inherit 'variable-pitch))

     ;;https://github.com/skeeto/elfeed
     (elfeed-search-feed-face                           (:foreground lavender))
     (elfeed-search-tag-face                            (:foreground green))

     ;; https://www.emacswiki.org/emacs/MessageMode
     (message-header-name                               (:foreground overlay2))
     (message-header-other                              (:foreground green))
     (message-header-xheader                            (:foreground rosewater))
     (message-header-to                                 (:foreground red))
     (message-header-cc                                 (:foreground lavender))
     (message-header-subject                            (:foreground yellow))

     ;; https://www.emacswiki.org/emacs/WritingMail
     (gnus-header-name                                  (:inherit 'message-header-name    :height 1.05 :bold t))
     (gnus-header-from                                  (:inherit 'message-header-to      :height 1.05 :bold t))
     (gnus-header-subject                               (:inherit 'message-header-subject :height 1.05 :bold t))
     (gnus-header-content                               (:inherit 'message-header-other   :height 1.05 :bold t))

     ;; https://github.com/alphapapa/ement.el
     (ement-room-mention                                (:foreground red))
     (ement-room-name                                   (:foreground lavender))
     (ement-room-self                                   (:foreground pink))
     (ement-room-self-message                           (:foreground text))
     (ement-room-quote                                  (:foreground lavender :italic t))
     (ement-room-list-direct                            (:foreground peach :italic t))
     (ement-room-list-favourite                         (:foreground flamingo))
     (ement-room-list-recent                            (:foreground yellow :bold t))
     (ement-room-list-space                             (:foreground mauve))
     (ement-room-list-name                              (:inherit 'ement-room-name :italic t))
     (ement-room-list-very-recent                       (:foreground red :bold t))
     (ement-room-reactions                              (:background base :foreground pink))

     ;; https://orgmode.org/org.html
     (org-block                                         (:inherit 'fixed-pitch :background mantle))
     (org-block-begin-line                              (:inherit 'fixed-pitch :background base   :foreground rosewater))
     (org-block-end-line                                (:inherit 'fixed-pitch :background base   :foreground maroon))
     (org-code                                          (:inherit 'fixed-pitch :background mantle :foreground teal))
     (org-date                                          (:foreground pink))
     (org-done                                          (:foreground green))
     (org-ellipsis                                      (:foreground overlay2 :bold t))
     (org-footnote                                      (:foreground flamingo))
     (org-headline-done                                 (:foreground green :strike-through t))
     (org-headline-todo                                 (:foreground yellow))
     (org-hide                                          (:background base :foreground base))
     (org-indent                                        (:background base :foreground base))
     (org-meta-line                                     (:foreground blue :bold t))
     (org-quote                                         (:foreground lavender :italic t))
     (org-tag                                           (:foreground mauve))
     (org-todo                                          (:foreground yellow :bold t))
     (org-verbatim                                      (:foreground sky))
     (org-upcoming-deadline                             (:foreground red))

     (org-document-title                                (:foreground maroon   :height 1.54 :bold t))
     (org-level-1                                       (:foreground red      :height 1.25 :bold t))
     (org-level-2                                       (:foreground mauve    :height 1.15 :bold t))
     (org-level-3                                       (:foreground flamingo :height 1.12))
     (org-level-4                                       (:foreground text     :height 1.09))
     (org-level-5                                       (:foreground text     :height 1.06))
     (org-level-6                                       (:foreground yellow))
     (org-level-7                                       (:foreground peach))
     (org-level-8                                       (:foreground maroon))

     ;; https://github.com/justbur/emacs-which-key
     (which-func                                        (:inherit 'font-lock-function-name-face :bold t))
     (which-key-command-description-face                (:foreground lavender))
     (which-key-group-description-face                  (:foreground pink))
     (which-key-key-face                                (:inherit 'font-lock-variable-name-face))
     (which-key-local-map-description-face              (:foreground yellow))
     (which-key-posframe                                (:background crust))
     (which-key-posframe-border                         (:background crust))

     ;; https://github.com/abo-abo/swiper
     (swiper-line-face                                  (:foreground yellow))

     (swiper-background-match-face-1                    (:background peach :foreground crust))
     (swiper-background-match-face-2                    (:background blue :foreground crust))
     (swiper-background-match-face-3                    (:background flamingo :foreground crust))
     (swiper-background-match-face-4                    (:background red :foreground crust))

     (swiper-match-face-1                               (:inherit 'swiper-background-match-face-1))
     (swiper-match-face-2                               (:inherit 'swiper-background-match-face-2))
     (swiper-match-face-3                               (:inherit 'swiper-background-match-face-3))
     (swiper-match-face-4                               (:inherit 'swiper-background-match-face-4))

     ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Xref.html
     (counsel-outline-default                           (:foreground yellow))
     (info-header-xref                                  (:foreground yellow))
     (xref-file-header                                  (:foreground yellow))
     (xref-match                                        (:foreground peach))

     ;; https://github.com/Fanael/rainbow-delimiters
     (rainbow-delimiters-base-error-face                (:foreground red :background yellow))
     (rainbow-delimiters-base-face                      (:foreground overlay2))
     (rainbow-delimiters-mismatched-face                (:foreground red :background yellow))
     (rainbow-delimiters-unmatched-face                 (:foreground green :background yellow))

     (rainbow-delimiters-depth-1-face                   (:foreground text))
     (rainbow-delimiters-depth-2-face                   (:foreground red))
     (rainbow-delimiters-depth-3-face                   (:foreground blue))
     (rainbow-delimiters-depth-4-face                   (:foreground yellow))
     (rainbow-delimiters-depth-5-face                   (:foreground green))
     (rainbow-delimiters-depth-6-face                   (:foreground mauve))
     (rainbow-delimiters-depth-7-face                   (:foreground peach))
     (rainbow-delimiters-depth-8-face                   (:foreground flamingo))
     (rainbow-delimiters-depth-9-face                   (:foreground teal))

     ;; https://www.emacswiki.org/emacs/ShowParenMode
     (show-paren-match                                  (:background red :foreground crust :bold t))
     (show-paren-match-expression                       (:background red :foreground crust :bold t))
     (show-paren-mismatch                               (:background red :foreground crust))

     ;; https://company-mode.github.io/manual
     (company-box-annotation                            (:foreground red :background surface1))
     (company-box-numbers                               (:foreground blue :background surface1))
     (company-preview                                   (:foreground blue))
     (company-preview-common                            (:background red :foreground crust))
     (company-preview-search                            (:inherit 'company-tooltip-search))
     (company-scrollbar-bg                              (:inherit 'tooltip))
     (company-scrollbar-fg                              (:background red))
     (company-template-field                            (:inherit 'match))
     (company-tooltip                                   (:background crust :foreground overlay2))
     (company-tooltip-annotation                        (:foreground green))
     (company-tooltip-common                            (:foreground red :distant-foreground red :bold t))
     (company-tooltip-mouse                             (:background nil :foreground crust :distant-foreground text))
     (company-tooltip-quick-access                      (:foreground blue :distant-foreground surface1))
     (company-tooltip-scrollbar-thumb                   (:background flamingo))
     (company-tooltip-scrollbar-track                   (:background peach))
     (company-tooltip-search                            (:background peach))
     (company-tooltip-selection                         (:background surface1 :foreground text :distant-foreground text :bold t))

     ;; https://github.com/casouri/eldoc-box
     (eldoc-box-body                                    (:background mantle :foreground lavender :italic t))
     (eldoc-box-border                                  (:background pink :foreground nil))

     ;; https://jblevins.org/projects/markdown-mode/
     (markdown-hr-face                                  (:background nil :foreground surface0))
     (markdown-header-face-1                            (:inherit 'markdown-header-face :height 1.25 :weight 'extra-bold))
     (markdown-header-face-2                            (:inherit 'markdown-header-face :height 1.15 :weight 'bold))
     (markdown-header-face-3                            (:inherit 'markdown-header-face :height 1.08 :weight 'bold))
     (markdown-header-face-4                            (:inherit 'markdown-header-face :height 1.00 :weight 'bold))
     (markdown-header-face-5                            (:inherit 'markdown-header-face :height 0.90 :weight 'bold))
     (markdown-header-face-6                            (:inherit 'markdown-header-face :height 0.75 :weight 'extra-bold))

     ;; https://www.emacswiki.org/emacs/FlyMake
     (flymake-errline                                   (:foreground red))
     (flymake-error                                     (:inherit 'error))
     (flymake-error-echo                                (:inherit 'errline))
     (flymake-note                                      (:foreground lavender))
     (flymake-warline                                   (:foreground peach))
     (flymake-warning                                   (:inherit 'warning))

     ;; https://www.flycheck.org/en/latest
     (flycheck-error-list-error                         (:inheirt 'error :bold t))
     (flycheck-error-list-info                          (:inherit 'info :bold t))
     (flycheck-error-list-warning                       (:inherit 'warning :bold t))

     (flycheck-fringe-error                             (:inherit 'error))
     (flycheck-fringe-info                              (:inherit 'info))
     (flycheck-fringe-warning                           (:inherit 'warning))

     (flycheck-inline-error                             (:foreground "black" :background red :height 128))
     (flycheck-inline-info                              (:foreground "black" :background blue :height 128))
     (flycheck-inline-warning                           (:foreground "black" :background yellow :height 128))

     (flycheck-posframe-background-face                 (:background crust))
     (flycheck-posframe-error-face                      (:background crust :inherit 'error))
     (flycheck-posframe-face                            (:background crust))
     (flycheck-posframe-info-face                       (:background crust :inherit 'info))
     (flycheck-posframe-warning-face                    (:background crust :inherit 'warning))

     ;; https://github.com/DarthFennec/highlight-indent-guides
     (highlight-indent-guides-character-face            (:foreground surface0))
     (highlight-indent-guides-even-face                 (:foreground surface0))
     (highlight-indent-guides-odd-face                  (:foreground surface1))
     (highlight-indent-guides-stack-character-face      (:foreground surface0))
     (highlight-indent-guides-stack-character-face      (:foreground surface0))
     (highlight-indent-guides-stack-even-face           (:foreground surface1))
     (highlight-indent-guides-stack-odd-face            (:foreground surface0))

     ;; https://lists.gnu.org/archive/html/help-gnu-emacs/2009-05/msg00013.html
     (highlight-numbers-face                            (:foreground pink))
     (highlight-operators-face                          (:foreground red))
     (highlight-quoted-symbol                           (:foreground maroon))
     (highlight-symbol-face                             (:background mantle :foreground green :weight 'semi-bold))

     ;; https://oremacs.com/swiper
     (ivy-action                                        (:background nil :foreground lavender))
     (ivy-confirm-face                                  (:foreground green))
     (ivy-current-match                                 (:background maroon :foreground crust :bold t))
     (ivy-grep-info                                     (:foreground lavender))
     (ivy-grep-line-number                              (:background nil :foreground peach))
     (ivy-grep-line-number                              (:foreground mauve))
     (ivy-minibuffer-match-face-1                       (:background nil :foreground blue :bold t))
     (ivy-minibuffer-match-face-2                       (:background nil :foreground green))
     (ivy-minibuffer-match-highlight                    (:foreground pink))

     ;; https://github.com/tumashu/ivy-posframe
     (ivy-posframe                                      (:background surface2))
     (ivy-posframe-border                               (:inherit 'ivy-posframe))

     ;; https://github.com/minad/vertico
     (vertico-current                                   (:foreground pink :bold t :italic t :background mantle :distant-background text))
     (vertico-group-separator                           (:foreground flamingo :strike-through t))
     (vertico-group-title                               (:foreground flamingo :bold t :italic t :height 150))
     (vertico-multiline                                 (:background red))
     (vertico-mouse                                     (:foreground crust :background flamingo))
     (vertico-posframe                                  (:foreground text :background surface0))
     (vertico-posframe-border                           (:background surface1))

     ;; https://github.com/minad/marginalia
     (marginalia-documentation                          (:foreground overlay1 :italic t))

     ;; https://github.com/minad/corfu
     (corfu-annotations                                 (:inherit 'marginalia-documentation))
     (corfu-current                                     (:inherit 'vertico-current))
     (corfu-border                                      (:background red))
     (corfu-bar                                         (:background peach))
     (corfu-default                                     (:background crust :foreground text))
     (corfu-popupinfo                                   (:background crust :foreground text :italic t))

     ;; https://joaotavora.github.io/eglot/
     (eglot-diagnostic-tag-unnecessary-face             (:foreground overlay1))
     ;; (eglot-type-hint-face)
     ;; (eglot-inlay-hint-face)
     ;; (eglot-parameter-hint-face)
     ;; (eglot-highlight-symbol-face)
     (eglot-diagnostic-tag-deprecated-face              (:foreground red))

     ;; https://github.com/oantolin/orderless
     (orderless-match-face-0                            (:foreground flamingo :bold t :underline t))
     (orderless-match-face-1                            (:inherit 'orderless-match-face-0 :foreground maroon))
     (orderless-match-face-2                            (:inherit 'orderless-match-face-0 :foreground mauve))
     (orderless-match-face-4                            (:inherit 'orderless-match-face-0 :foreground lavender))
     (comint-highlight-prompt                           (:background peach :foreground crust))

     ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Completion-Variables.html
     (completions-annotations                           (:inherit 'marginalia-documentation))
     (completions-highlight                             (:foreground mauve :italic t))
     (completions-common-part                           (:foreground yellow  :bold t :italic t :distant-foreground mantle :distant-background green))
     (completions-first-difference                      (:foreground red :strike-through t))

     ;; https://github.com/minad/consult
     (consult-file                                      (:foreground overlay2 :distant-foreground crust))
     (consult-projectile-projects                       (:foreground lavender))

     ;; https://emacs-lsp.github.io/lsp-mode/page/settings/headerline/
     (lsp-headerline-breadcrumb-path-error-face         (:underline (:color maroon :style 'wave) :foreground overlay2 :background crust))
     (lsp-headerline-breadcrumb-path-face               (:background crust))
     (lsp-headerline-breadcrumb-path-hint-face          (:background crust))
     (lsp-headerline-breadcrumb-path-info-face          (:background crust))
     (lsp-headerline-breadcrumb-separator-face          (:background crust))
     (lsp-headerline-breadcrumb-symbols-face            (:background crust))
     (lsp-headerline-breadcrumb-project-prefix-face     (:background crust))
     (lsp-headerline-breadcrumb-symbols-error-face      (:foreground red))

     ;; https://github.com/emacs-lsp/lsp-ui
     (lsp-ui-doc-background                             (:background crust :foreground red))
     (lsp-ui-doc-header                                 (:background crust :foreground red))
     (lsp-ui-doc-border                                 (:background nil :foreground nil))
     (lsp-ui-peek-filename                              (:foreground teal))
     (lsp-ui-sideline-code-action                       (:foreground yellow))
     (lsp-ui-sideline-current-symbol                    (:foreground sky))
     (lsp-ui-sideline-symbol                            (:foreground overlay1))

     ;; https://github.com/emacs-dashboard/emacs-dashboard
     (dashboard-banner-logo-title                       (:foreground pink :height 200 :weight 'regular :italic t))
     (dashboard-footer-face                             (:foreground flamingo :height 135 :bold t :italic t))
     (dashboard-heading                                 (:foreground mauve :height 150 :weight 'regular :bold t))
     (dashboard-items-face                              (:foreground lavender :bold nil))
     (dashboard-navigator                               (:foreground flamingo :weight 'semi-bold :italic nil))
     (dashboard-no-items-face                           (:foreground overlay1))

     ;; https://github.com/domtronn/all-the-icons.el
     (all-the-icons-dgreen                              (:foreground green))
     (all-the-icons-green                               (:foreground green))
     (all-the-icons-dpurple                             (:foreground mauve))
     (all-the-icons-purple                              (:foreground mauve))

     ;; https://github.com/Alexander-Miller/treemacs
     (treemacs-directory-collapsed-face                 (:foreground text))
     (treemacs-directory-face                           (:foreground text))
     (treemacs-file-face                                (:foreground text))

     (treemacs-git-added-face                           (:foreground peach))
     (treemacs-git-ignored-face                         (:foreground overlay2))
     (treemacs-git-modified-face                        (:foreground maroon))
     (treemacs-git-renamed-face                         (:foreground rosewater))
     (treemacs-git-renamed-face                         (:foreground text))
     (treemacs-git-unmodified-face                      (:foreground text))

     ;; https://github.com/hlissner/emacs-solaire-mode
     (solaire-default-face                              (:background mantle))

     ;; https://www.emacswiki.org/emacs/EdiffMode
     (diff-added                                        (:background green :foreground mantle))
     (diff-changed                                      (:background yellow :foreground mantle))
     (diff-removed                                      (:background red :foreground mantle))
     (diff-error                                        (:background base :foreground red))

     (diff-hl-margin-change                             (:background blue :foreground mantle))
     (diff-hl-margin-delete                             (:background red  :foreground mantle))
     (diff-hl-margin-insert                             (:background yellow :foreground mantle))

     ;; (related) https://github.com/emacs-evil/evil
     (evil-ex-info                                      (:foreground blue))
     (evil-ex-lazy-highlight                            (:inherit 'isearch :bold t))
     (evil-ex-substitute-matches                        (:foreground red :strike-through t))
     (evil-ex-substitute-replacement                    (:foreground yellow :bold t))

     ;; https://github.com/blorbx/evil-quickscope
     (evil-quickscope-first-face                        (:foreground yellow :underline t))
     (evil-quickscope-second-face                       (:foreground peach :underline t))

     ;; https://github.com/edkolev/evil-goggles
     (evil-goggles-default-face                         (:foreground crust))
     (evil-goggles-join-face                            (:background yellow))
     (evil-goggles-delete-face                          (:background red))
     (evil-goggles-paste-face                           (:background green))
     (evil-goggles-indent-face                          (:background lavender))
     (evil-goggles-set-marker-face                      (:background maroon))
     (evil-goggles-yank-face                            (:background rosewater))

     ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Term-Mode.html
     (term                                              (:background crust :foreground text))
     (term-color-blue                                   (:background blue :foreground blue))
     (term-color-bright-blue                            (:inherit 'term-color-blue))
     (term-color-cyan                                   (:background sky :foreground sky))
     (term-color-bright-cyan                            (:background sky :foreground sky))
     (term-color-bright-crust                           (:background mantle :foreground red))
     (term-color-green                                  (:background green :foreground green))
     (term-color-bright-green                           (:inherit 'term-color-green))
     (term-color-magenta                                (:background mauve :foreground mauve))
     (term-color-bright-magenta                         (:background mauve :foreground mauve))
     (term-color-red                                    (:background red :foreground red))
     (term-color-bright-red                             (:background maroon :foreground maroon))
     (term-color-rosewater                              (:background text :foreground text))
     (term-color-bright-rosewater                       (:background rosewater :foreground rosewater))
     (term-color-yellow                                 (:background yellow :foreground yellow))
     (term-color-bright-yellow                          (:background yellow :foreground yellow))
     (term-underline                                    (:background mauve :foreground blue))

     ;; https://github.com/akermu/emacs-libvterm
     (vterm-color-crust                                 (:background mantle :foreground mantle))
     (vterm-color-blue                                  (:background blue :foreground blue))
     (vterm-color-cyan                                  (:background sky :foreground sky))
     (vterm-color-green                                 (:background green :foreground green))
     (vterm-color-magenta                               (:background mauve :foreground maroon))
     (vterm-color-yellow                                (:background peach :foreground yellow))
     (vterm-color-red                                   (:background red :foreground red))
     (vterm-color-rosewater                             (:background text :foreground text))

     ;; https://www.emacswiki.org/emacs/AnsiColor
     (ansi-color-black                                  (:foreground overlay1))
     (ansi-color-bright-black                           (:foreground overlay1))
     (ansi-color-green                                  (:foreground green))
     (ansi-color-bright-green                           (:foreground green))
     (ansi-color-cyan                                   (:foreground sky))
     (ansi-color-bright-cyan                            (:foreground sky))
     (ansi-color-magenta                                (:foreground mauve))
     (ansi-color-bright-magenta                         (:foreground mauve))
     (ansi-color-blue                                   (:foreground blue))
     (ansi-color-bright-blue                            (:foreground blue))
     (ansi-color-red                                    (:foreground red))
     (ansi-color-bright-red                             (:foreground red))
     (ansi-color-yellow                                 (:foreground yellow))
     (ansi-color-bright-yellow                          (:foreground yellow))
     (ansi-color-white                                  (:foreground text))
     (ansi-color-bright-white                           (:foreground text))

     ;; https://github.com/auto-complete/popup-el
     (popup-face                                        (:inherit 'tooltip))
     (popup-selection-face                              (:inherit 'tooltip))
     (popup-tip-face                                    (:inherit 'tooltip))

     ;; https://github.com/emacsorphanage/anzu
     (anzu-match-1                                      (:foreground green :background crust))
     (anzu-match-2                                      (:foreground yellow :background crust))
     (anzu-match-3                                      (:foreground teal :background crust))

     (anzu-mode-line                                    (:foreground crust :background mauve))
     (anzu-mode-no-match                                (:foreground text :background red))
     (anzu-replace-to                                   (:foreground yellow :background surface2))

     ;; https://github.com/winterTTr/ace-jump-mode
     (ace-jump-face-background                          (:foreground overlay2))
     (ace-jump-face-foreground                          (:foreground red :background crust :bold t))

     ;; https://github.com/abo-abo/hydra
     (hydra-face-amaranth                               (:foreground mauve))
     (hydra-face-blue                                   (:foreground blue))
     (hydra-face-pink                                   (:foreground pink))
     (hydra-face-red                                    (:foreground red))
     (hydra-face-teal                                   (:foreground teal))

     ;; https://github.com/Ladicle/hydra-posframe
     (hydra-posframe-face                               (:background mantle))
     (hydra-posframe-border-face                        (:background pink))

     ;; Bookmarks
     (bm-fringe-face                                    (:background red :foreground crust))
     (bm-fringe-persistent-face                         (:background red :foreground crust))

     ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Tab-Line.html
     (tab-line                                          (:background mantle))
     (tab-line-tab-modified                             (:background base :foreground yellow :underline (:color peach)))
     (tab-line-tab-current                              (:background base :foreground pink :underline (:color maroon)))
     (tab-line-tab-inactive                             (:background base :foreground overlay2))
     (tab-line-tab-inactive-alternate                   (:inherit 'tab-line-tab-inactive))
     (tab-line-close-highlight                          (:background nil :foreground red))
     (tab-line-highlight                                (:background base :foreground flamingo :underline t))
     (tab-line-tab-special                              (:background base :foreground green :italic t))
     (tab-line-tab-group                                (:background base :foreground blue :strike-through t))

     ;; https://www.emacswiki.org/emacs/TabBarMode
     (tab-bar                                           (:background mantle  :foreground surface0))
     (tab-bar-tab                                       (:background base :foreground pink :underline (:color maroon) :bold t))
     (tab-bar-tab-inactive                              (:foreground overlay0))

     ;; https://github.com/ema2159/centaur-tabs
     (centaur-tabs-active-bar-face                      (:background maroon :foreground text))
     (centaur-tabs-selected                             (:background base :foreground pink))
     (centaur-tabs-selected-modified                    (:background base :foreground yellow :underline (:color peach)))
     (centaur-tabs-modified-marker-selected             (:background mantle :foreground peach :underline t))
     (centaur-tabs-close-selected                       (:inherit 'centaur-tabs-selected))

     (centaur-tabs-unselected                           (:background mantle :foreground overlay2))
     (centaur-tabs-unselected-modified                  (:background mantle :foreground mauve))
     (centaur-tabs-modified-marker-unselected           (:background mantle :foreground overlay2))
     (centaur-tabs-close-unselected                     (:background mantle :foreground overlay2))

     (centaur-tabs-close-mouse-face                     (:background nil :foreground red))
     (centaur-tabs-default                              (:background mantle :foreground mantle))
     (centaur-tabs-name-mouse-face                      (:foreground blue :bold t))

     ;; https://github.com/Artawower/blamer.el
     (blamer-face                                       (:foreground mauve :italic t))
     (blamer-pretty-border-face                         (:foreground pink))
     (blamer-pretty-meta-data-face                      (:foreground peach))
     (blamer-pretty-meta-keywords-face                  (:foreground blue))
     (blamer-pretty-commit-message-face                 (:foreground green :italic t))

     ;; https://github.com/magit/magit
     (magit-section-heading                             (:foreground pink))

     ;; https://github.com/emacsorphanage/git-gutter
     (git-gutter:added                                  (:foreground green))
     (git-gutter:deleted                                (:foreground red))
     (git-gutter:modified                               (:foreground blue))

     ;; https://github.com/minad/goggles
     (goggles-added                                     (:background green))
     (goggles-changed                                   (:background yellow))
     (goggles-removed                                   (:background red))

     ;; https://tree-sitter.github.io/tree-sitter
     (tree-sitter-hl-face:function                      (:foreground blue))
     (tree-sitter-hl-face:function.call                 (:foreground sapphire :italic t :underline t))
     (tree-sitter-hl-face:function.builtin              (:foreground sky))
     (tree-sitter-hl-face:function.special              (:foreground text :italic t :bold t))
     (tree-sitter-hl-face:function.macro                (:foreground sapphire))

     (tree-sitter-hl-face:method                        (:foreground blue :italic t))
     (tree-sitter-hl-face:method.call                   (:foreground peach))

     (tree-sitter-hl-face:type                          (:foreground maroon :bold t))
     (tree-sitter-hl-face:type.parameter                (:foreground sapphire :italic t))
     (tree-sitter-hl-face:type.argument                 (:foreground subtext0 :background peach))
     (tree-sitter-hl-face:type.builtin                  (:foreground peach :italic t))
     (tree-sitter-hl-face:type.super                    (:foreground green :bold t))
     (tree-sitter-hl-face:constructor                   (:foreground teal :weight 'semi-bold))

     (tree-sitter-hl-face:variable                      (:foreground rosewater))
     (tree-sitter-hl-face:variable.parameter            (:foreground flamingo))
     (tree-sitter-hl-face:variable.builtin              (:foreground mauve :bold t))
     (tree-sitter-hl-face:variable.special              (:foreground mauve))
     (tree-sitter-hl-face:property                      (:foreground rosewater))
     (tree-sitter-hl-face:property.definition           (:foreground rosewater :italic t))

     (tree-sitter-hl-face:comment                       (:foreground surface2 :italic t))
     (tree-sitter-hl-face:doc                           (:foreground blue :italic t))
     (tree-sitter-hl-face:string                        (:foreground green :italic t))
     (tree-sitter-hl-face:string.special                (:foreground green :italic t))
     (tree-sitter-hl-face:escape                        (:foreground yellow :background surface1))
     (tree-sitter-hl-face:embedded                      (:foreground teal))

     (tree-sitter-hl-face:keyword                       (:foreground maroon :bold t))
     (tree-sitter-hl-face:operator                      (:foreground blue :bold t))
     (tree-sitter-hl-face:label                         (:inherit 'tree-sitter-hl-face:keyword :italic t))
     (tree-sitter-hl-face:constant                      (:foreground yellow))
     (tree-sitter-hl-face:constant.builtin              (:foreground yellow :weight 'semi-bold))
     (tree-sitter-hl-face:number                        (:foreground peach))

     (tree-sitter-hl-face:punctuation                   (:foreground maroon))
     (tree-sitter-hl-face:punctuation.bracket           (:foreground subtext1))
     (tree-sitter-hl-face:punctuation.delimiter         (:foreground text :bold t))
     (tree-sitter-hl-face:punctuation.special           (:foreground yellow))

     (tree-sitter-hl-face:case-pattern                  (:foreground peach))
     (tree-sitter-hl-face:keyword.compiler              (:foreground overlay2 :bold t :italic t))

     ;; Custom for pinkus tree-sitter-swift
     (tree-sitter-hl-face:include                       (:foreground mauve :italic t))
     (tree-sitter-hl-face:parameter                     (:foreground sky))
     (tree-sitter-hl-face:repeat                        (:foreground blue :bold t))
     (tree-sitter-hl-face:boolean                       (:foreground yellow))
     (tree-sitter-hl-face:keyword.return                (:foreground maroon :bold t :italic t))
     (tree-sitter-hl-face:keyword.operator              (:foreground sapphire :bold t))
     (tree-sitter-hl-face:keyword.function              (:foreground maroon :bold t))
     (tree-sitter-hl-face:conditional                   (:foreground lavender :bold t))

     (swift-mode:preprocessor-keyword-face              (:foreground text :italic t))
     (swift-mode:property-access-face                   (:foreground subtext1))
     (swift-mode:builtin-property-face                  (:foreground teal))
     (swift-mode:builtin-enum-case-face                 (:foreground teal))
     (swift-mode:builtin-method-trailing-closure-face   (:foreground teal))
     (swift-mode:builtin-function-trailing-closure-face (:foreground teal)))

    ,@body))

(provide 'catppuccin)
;;; catppuccin.el ends here
