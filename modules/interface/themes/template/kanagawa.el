;;; package: --- Kanagawa template for our theme variants -*- lexical-binding: t;; -*-

;;; Commentary:
;;; [1]: https://github.com/rebelot/kanagawa.nvim

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

(defmacro kanagawa-deftheme (name description palette &rest body)
  `(autothemer-deftheme
    ,name
    ,description
    ,palette
    (;; Built-in Emacs faces
     (default                                       (:background sumiInk-1b :foreground fujiWhite))
     (border                                        (:background sumiInk-1b :foreground sumiInk-0))
     (button                                        (:foreground waveAqua2))
     (child-frame                                   (:background sumiInk-0 :foreground sumiInk-0))
     (child-frame-border                            (:background sumiInk-0 :foreground sumiInk-0))
     (cursor                                        (:background autumnGreen :foreground sumiInk-0 :bold t))
     (custom-link                                   (:foreground crystalBlue))
     (error                                         (:foreground samuraiRed))
     (fringe                                        (:foreground sumiInk-3))
     (glyph-face                                    (:background sumiInk-4))
     (glyphless-char                                (:foreground sumiInk-4))
     (header-line                                   (:background sumiInk-0))
     (highlight                                     (:background comet :foreground springViolet1))
     (hl-line                                       (:background sumiInk-1))
     (homoglyph                                     (:foreground lightBlue))
     (internal-border                               (:background sumiInk-1b))
     (line-number                                   (:foreground sumiInk-4))
     (line-number-current-line                      (:foreground springViolet2 :background sumiInk-2 :bold t))
     (link                                          (:inherit 'custom-link))
     (match                                         (:background carpYellow :foreground sumiInk-0))
     (menu                                          (:background sumiInk-0 :foreground fujiWhite))
     (mode-line                                     (:background sumiInk-0 :foreground fujiWhite :bold t))
     (mode-line-inactive                            (:background nil :foreground sumiInk-4 :bold nil))
     (mode-line-active                              (:background sumiInk-0 :foreground old-white :bold nil))
     (mode-line-highlight                           (:foreground boatYellow2))
     (mode-line-buffer-id                           (:foreground waveAqua2 :bold t))
     (numbers                                       (:background sakuraPink))
     (region                                        (:background waveBlue-2 :foreground fujiWhite))
     (separator-line                                (:background sumiInk-0))
     (shadow                                        (:background sumiInk-0))
     (success                                       (:foreground waveAqua2))
     (vertical-border                               (:foreground nil))
     (warning                                       (:foreground roninYellow))
     (window-border                                 (:background sumiInk-1b))
     (window-divider                                (:foreground sumiInk-2))

     ;; https://www.emacswiki.org/emacs/WhiteSpace
     (whitespace-tab                                (:foreground sumiInk-2))
     (whitespace-space                              (:foreground sumiInk-2))
     (whitespace-space-before-tab                   (:inherit 'whitespace-space))
     (whitespace-space-after-tab                    (:inherit 'whitespace-space))
     (whitespace-empty                              (:foreground carpYellow))
     (whitespace-line                               (:foreground dragonBlue))
     (whitespace-newline                            (:foreground sumiInk-2))
     (whitespace-missing-newline-at-eof             (:foreground surimiOrange))
     (whitespace-indentation                        (:foreground sumiInk-2))
     (whitespace-big-indent                         (:foreground waveRed))
     (whitespace-trailing                           (:foreground sakuraPink))

     (trailing-whitespace                           (:background comet))

     ;; https://www.emacswiki.org/emacs/FontLockMode
     (elisp-shorthand-font-lock-face                (:foreground fujiWhite))
     (font-lock-builtin-face                        (:foreground springBlue))
     (font-lock-comment-delimiter-face              (:foreground fujiGray :italic t))
     (font-lock-comment-face                        (:foreground fujiGray :italic t))
     (font-lock-constant-face                       (:foreground carpYellow))
     (font-lock-doc-face                            (:foreground comet))
     (font-lock-doc-markup-face                     (:foreground comet))
     (font-lock-function-name-face                  (:foreground crystalBlue))
     (font-lock-keyword-face                        (:foreground oniViolet :weight 'semi-bold))
     (font-lock-negation-char-face                  (:foreground peachRed))
     (font-lock-preprocessor-face                   (:foreground boatYellow2))
     (font-lock-reference-face                      (:foreground peachRed))
     (font-lock-regexp-grouping-backslash           (:foreground boatYellow2))
     (font-lock-string-face                         (:foreground springGreen :italic t))
     (font-lock-type-face                           (:foreground waveAqua2))
     (font-lock-variable-name-face                  (:foreground autumnGreen))
     (font-lock-warning-face                        (:foreground roninYellow))

     ;; https://www.emacswiki.org/emacs/MiniBuffer#minibuffer
     (minibuffer-prompt                             (:background sumiInk-1 :foreground carpYellow))
     (minibuffer-prompt-end                         (:background sumiInk-1 :foreground autumnRed))

     (epa-mark                                      (:foreground waveRed))
     (info-xref                                     (:foreground carpYellow))

     ;; https://www.emacswiki.org/emacs/DiredMode
     (dired-mark                                    (:foreground waveRed))
     (dired-ignored                                 (:background autumnYellow))

     ;; https://github.com/purcell/diredfl
     (diredfl-compressed-file-name                  (:foreground autumnGreen))
     (diredfl-compressed-file-suffix                (:foreground autumnGreen))
     (diredfl-deletion                              (:background autumnRed :foreground autumnYellow))
     (diredfl-dir-name                              (:foreground waveAqua2))
     (diredfl-dir-heading                           (:foreground waveAqua1 :height 1.15 :bold t :italic t))
     (diredfl-file-name                             (:foreground autumnGreen))
     (diredfl-file-suffix                           (:foreground carpYellow))
     (diredfl-flag-mark                             (:background waveAqua1 :foreground carpYellow))
     (diredfl-link-prev                             (:foreground springBlue))
     (diredfl-number                                (:foreground springViolet1))

     (diredfl-no-priv                               (:foreground sumiInk-4))
     (diredfl-dir-priv                              (:foreground crystalBlue))
     (diredfl-read-priv                             (:foreground springGreen))
     (diredfl-write-priv                            (:foreground boatYellow2))
     (diredfl-exec-priv                             (:foreground peachRed))

     ;; https://github.com/alexluigit/dirvish
     (dirvish-git-commit-message-face               (:background autumnYellow :foreground sumiInk-1b))
     (dirvish-hl-line                               (:background waveAqua1 :foreground sumiInk-1b))
     (dirvish-subtree-guide                         (:foreground sumiInk-2))
     (dirvish-subtree-state                         (:foreground sumiInk-2))
     (dirvish-vc-needs-merge-face                   (:background autumnRed :foreground sumiInk-1b))

     ;; https://github.com/jaypei/emacs-neotree
     (neo-dir-link-face                             (:foreground crystalBlue :weight 'bold))
     (neo-expand-btn-face                           (:foreground boatYellow2))
     (neo-file-link-face                            (:foreground fujiWhite))
     (neo-root-dir-face                             (:foreground oniViolet :weight 'bold))

     (neo-icon-dir-face                             (:foreground waveAqua1))
     (neo-icon-file-face                            (:foreground fujiWhite))
     (neo-icon-tag-directory-face                   (:foreground waveBlue-1))
     (neo-icon-tag-file-face                        (:foreground waveAqua1))
     (neo-icon-tag-text-face                        (:foreground fujiGray))

     (neo-vc-added-face                             (:foreground springGreen))
     (neo-vc-conflict-face                          (:foreground samuraiRed))
     (neo-vc-default-face                           (:foreground fujiWhite))
     (neo-vc-edited-face                            (:foreground autumnYellow))
     (neo-vc-ignored-face                           (:foreground sumiInk-4))
     (neo-vc-missing-face                           (:foreground samuraiRed :weight 'bold))
     (neo-vc-needs-merge-face                       (:foreground waveRed :weight 'bold))
     (neo-vc-removed-face                           (:foreground autumnRed :weight 'bold))
     (neo-vc-unlocked-changes-face                  (:foreground roninYellow))
     (neo-vc-unregistered-face                      (:foreground fujiGray))
     (neo-vc-up-to-date-face                        (:foreground fujiWhite))

     ;; https://github.com/nex3/perspective-el
     (persp-selected-face                           (:foreground crystalBlue :weight 'bold :italic t))

     ;; https://www.emacswiki.org/emacs/Iedit
     (iedit-occurrence                              (:background springBlue :foreground sumiInk-1))
     (iedit-read-only-occurrence                    (:background carpYellow :foreground sumiInk-1))

     ;; https://github.com/seagle0128/doom-modeline
     (doom-modeline-battery-charging                (:foreground fujiGray))
     (doom-modeline-battery-critical                (:foreground peachRed))
     (doom-modeline-battery-error                   (:foreground peachRed))
     (doom-modeline-battery-full                    (:foreground waveAqua2))
     (doom-modeline-battery-normal                  (:foreground springViolet1))
     (doom-modeline-battery-warning                 (:foreground springGreen))

     (doom-modeline-evil-emacs-state                (:foreground crystalBlue))
     (doom-modeline-evil-insert-state               (:foreground peachRed))
     (doom-modeline-evil-motion-state               (:foreground lightBlue))
     (doom-modeline-evil-normal-state               (:foreground lightBlue))
     (doom-modeline-evil-operator-state             (:foreground crystalBlue))
     (doom-modeline-evil-replace-state              (:foreground roninYellow))
     (doom-modeline-evil-visual-state               (:foreground springGreen))

     (doom-modeline-bar                             (:background sumiInk-1b))
     (doom-modeline-buffer-file                     (:inherit 'bold :foreground oniViolet))
     (doom-modeline-buffer-major-mode               (:foreground waveAqua2 :bold t))
     (doom-modeline-buffer-modified                 (:inherit 'bold :foreground carpYellow))
     (doom-modeline-buffer-path                     (:inherit 'bold :foreground waveAqua2))
     (doom-modeline-error                           (:background peachRed))
     (doom-modeline-info                            (:bold t :foreground lightBlue))
     (doom-modeline-panel                           (:inherit 'bold :background boatYellow2 :foreground sumiInk-2))
     (doom-modeline-project-dir                     (:bold t :foreground surimiOrange))
     (doom-modeline-project-dir                     (:bold t :foreground waveAqua2))
     (doom-themes-visual-bell                       (:background autumnRed))

     ;; https://github.com/dbordak/telephone-line
     (telephone-line-accent-active                  (:background sumiInk-1 :foreground dragonBlue :weight 'bold :italic t))
     (telephone-line-accent-inactive                (:background sumiInk-1 :foreground comet))
     (telephone-line-projectile                     (:foreground springGreen :weight 'bold :italic t))

     (telephone-line-evil-normal                    (:background autumnGreen :foreground sumiInk-1 :bold t))
     (telephone-line-evil-visual                    (:background crystalBlue :foreground sumiInk-1 :bold t))
     (telephone-line-evil-motion                    (:background oniViolet :foreground sumiInk-1 :bold t))
     (telephone-line-evil-insert                    (:background autumnYellow :foreground sumiInk-1 :bold t))

     ;; https://www.emacswiki.org/emacs/eww
     (eww-form-file                                 (:background sumiInk-1b :foreground autumnYellow))
     (eww-form-text                                 (:background sumiInk-1b :foreground fujiWhite))
     (eww-form-select                               (:background sumiInk-1b :foreground waveAqua2))
     (eww-form-submit                               (:background sumiInk-1b :foreground carpYellow))
     (eww-form-checkbox                             (:background sumiInk-1b :foreground crystalBlue))
     (eww-form-textarea                             (:background sumiInk-1b :foreground fujiWhite))
     (eww-valid-certificate                         (:foreground autumnGreen :italic t))
     (eww-invalid-certificate                       (:foreground autumnRed :italic t))

     ;; https://www.gnu.org/software/emacs/manual/html_node/eww/Advanced.html
     (shr-h1                                        (:inherit 'org-document-title))
     (shr-h2                                        (:inherit 'org-level-1))
     (shr-h3                                        (:inherit 'org-level-2))
     (shr-h4                                        (:inherit 'org-level-3))
     (shr-h5                                        (:inherit 'org-level-4))
     (shr-h6                                        (:inherit 'org-level-5))
     (shr-text                                      (:inherit 'variable-pitch))

     ;;https://github.com/skeeto/elfeed
     (elfeed-search-feed-face                       (:foreground springViolet1))
     (elfeed-search-tag-face                        (:foreground waveAqua2))

     ;; https://www.emacswiki.org/emacs/MessageMode
     (message-header-name                           (:foreground sumiInk-4))
     (message-header-other                          (:foreground autumnGreen))
     (message-header-xheader                        (:foreground old-white))
     (message-header-to                             (:foreground autumnRed))
     (message-header-cc                             (:foreground sakuraPink))
     (message-header-subject                        (:foreground autumnYellow))

     ;; https://www.emacswiki.org/emacs/WritingMail
     (gnus-header-name                              (:inherit 'message-header-name    :height 1.05 :bold t))
     (gnus-header-from                              (:inherit 'message-header-to      :height 1.05 :bold t))
     (gnus-header-subject                           (:inherit 'message-header-subject :height 1.05 :bold t))
     (gnus-header-content                           (:inherit 'message-header-other   :height 1.05 :bold t))

     ;; https://github.com/alphapapa/ement.el
     (ement-room-list-favourite                     (:foreground dragonBlue))
     (ement-room-list-space                         (:foreground crystalBlue))
     (ement-room-list-recent                        (:foreground roninYellow))
     (ement-room-list-very-recent                   (:foreground autumnRed))
     (ement-room-mention                            (:foreground autumnRed))
     (ement-room-name                               (:foreground carpYellow ))
     (ement-room-list-name                          (:foreground boatYellow2))
     (ement-room-list-direct                        (:foreground oniViolet))
     (ement-room-self                               (:foreground autumnGreen))
     (ement-room-self-message                       (:foreground fujiWhite))
     (ement-room-quote                              (:foreground oniViolet))
     (ement-room-reactions                          (:background sumiInk-1b :foreground autumnYellow))

     ;; https://orgmode.org/org.html
     (org-block                                     (:inherit 'fixed-pitch :background sumiInk-0))
     (org-block-begin-line                          (:inherit 'fixed-pitch :background winterBlue :foreground springBlue :weight 'bold :italic t))
     (org-block-end-line                            (:inherit 'fixed-pitch :background winterRed  :foreground waveRed    :weight 'bold :italic t))
     (org-code                                      (:inherit 'fixed-pitch :foreground carpYellow))
     (org-date                                      (:foreground springBlue))
     (org-done                                      (:foreground dragonBlue))
     (org-ellipsis                                  (:foreground waveBlue-2 :bold t))
     (org-footnote                                  (:foreground waveAqua2))
     (org-headline-done                             (:foreground dragonBlue :strike-through t))
     (org-headline-todo                             (:foreground sumiInk-2))
     (org-hide                                      (:background sumiInk-1b  :foreground sumiInk-1b))
     (org-indent                                    (:background sumiInk-1b  :foreground sumiInk-1b))
     (org-meta-line                                 (:background winterGreen :foreground springGreen))
     (org-quote                                     (:background sumiInk-1b  :foreground springGreen :italic t))
     (org-tag                                       (:foreground autumnGreen))
     (org-todo                                      (:foreground autumnYellow :bold t))
     (org-verbatim                                  (:foreground crystalBlue))
     (org-upcoming-deadline                         (:foreground peachRed))

     (org-document-title                            (:foreground autumnGreen   :height 1.54 :bold t))
     (org-level-1                                   (:foreground peachRed      :height 1.25 :bold t))
     (org-level-2                                   (:foreground springViolet2 :height 1.15 :bold t))
     (org-level-3                                   (:foreground boatYellow2   :height 1.12))
     (org-level-4                                   (:foreground fujiWhite     :height 1.09))
     (org-level-5                                   (:foreground fujiWhite     :height 1.06))
     (org-level-6                                   (:foreground carpYellow))
     (org-level-7                                   (:foreground surimiOrange))
     (org-level-8                                   (:foreground springGreen))

     ;; https://github.com/justbur/emacs-which-key
     (which-func                                    (:inherit 'font-lock-function-name-face :bold t))
     (which-key-command-description-face            (:foreground crystalBlue))
     (which-key-group-description-face              (:foreground waveRed))
     (which-key-key-face                            (:foreground waveRed))
     (which-key-local-map-description-face          (:foreground carpYellow))
     (which-key-posframe                            (:background waveBlue-1))
     (which-key-posframe-border                     (:background waveBlue-1))

     ;; https://github.com/abo-abo/swiper
     (swiper-line-face                              (:foreground carpYellow))
     (swiper-background-match-face-1                (:background surimiOrange :foreground sumiInk-0))
     (swiper-background-match-face-2                (:background crystalBlue :foreground sumiInk-0))
     (swiper-background-match-face-3                (:background boatYellow2 :foreground sumiInk-0))
     (swiper-background-match-face-4                (:background peachRed :foreground sumiInk-0))
     (swiper-match-face-1                           (:inherit 'swiper-background-match-face-1))
     (swiper-match-face-2                           (:inherit 'swiper-background-match-face-2))
     (swiper-match-face-3                           (:inherit 'swiper-background-match-face-3))
     (swiper-match-face-4                           (:inherit 'swiper-background-match-face-4))

     ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Xref.html
     (counsel-outline-default                       (:foreground carpYellow))
     (info-header-xref                              (:foreground carpYellow))
     (xref-file-header                              (:foreground carpYellow))
     (xref-match                                    (:foreground carpYellow))

     ;; https://github.com/Fanael/rainbow-delimiters
     (rainbow-delimiters-base-error-face            (:foreground peachRed))
     (rainbow-delimiters-base-face                  (:foreground sumiInk-4))
     (rainbow-delimiters-mismatched-face            (:foreground peachRed))
     (rainbow-delimiters-unmatched-face             (:foreground waveAqua2))

     (rainbow-delimiters-depth-1-face               (:foreground springViolet2))
     (rainbow-delimiters-depth-2-face               (:foreground dragonBlue))
     (rainbow-delimiters-depth-3-face               (:foreground springViolet1))
     (rainbow-delimiters-depth-4-face               (:foreground springGreen))
     (rainbow-delimiters-depth-5-face               (:foreground waveAqua2))
     (rainbow-delimiters-depth-6-face               (:foreground carpYellow))
     (rainbow-delimiters-depth-7-face               (:foreground waveRed))
     (rainbow-delimiters-depth-8-face               (:foreground lightBlue))
     (rainbow-delimiters-depth-9-face               (:foreground springViolet2))

     ;; https://www.emacswiki.org/emacs/ShowParenMode
     (show-paren-match                              (:background waveAqua1 :foreground sumiInk-0 :bold t))
     (show-paren-match-expression                   (:background waveAqua1 :foreground sumiInk-0 :bold t))
     (show-paren-mismatch                           (:background peachRed :foreground old-white))
     (tooltip                                       (:foreground sumiInk-0 :background carpYellow :bold t))

     ;; https://company-mode.github.io/manual
     (company-preview                               (:foreground carpYellow))
     (company-preview-common                        (:foreground peachRed :bold t))
     (company-preview-search                        (:inherit 'company-tooltip-search))
     (company-scrollbar-bg                          (:inherit 'tooltip))
     (company-scrollbar-fg                          (:background peachRed))
     (company-template-field                        (:inherit 'match))
     (company-tooltip                               (:background sumiInk-2))
     (company-tooltip-annotation                    (:foreground peachRed :distant-foreground sumiInk-1))
     (company-tooltip-common                        (:foreground autumnYellow))
     (company-tooltip-mouse                         (:background sumiInk-2 :foreground sumiInk-0 :distant-foreground fujiWhite))
     (company-tooltip-quick-access                  (:foreground springViolet2))
     (company-tooltip-scrollbar-thumb               (:background autumnRed))
     (company-tooltip-scrollbar-track               (:background sumiInk-2))
     (company-tooltip-search                        (:background carpYellow :foreground sumiInk-0 :distant-foreground fujiWhite))
     (company-tooltip-selection                     (:background peachRed :foreground winterRed :bold t))

     ;; https://jblevins.org/projects/markdown-mode/
     (markdown-hr-face                              (:foreground sumiInk-2))
     (markdown-header-face-1                        (:inherit 'markdown-header-face :height 1.25 :weight 'extra-bold))
     (markdown-header-face-2                        (:inherit 'markdown-header-face :height 1.15 :weight 'bold))
     (markdown-header-face-3                        (:inherit 'markdown-header-face :height 1.08 :weight 'bold))
     (markdown-header-face-4                        (:inherit 'markdown-header-face :height 1.00 :weight 'bold))
     (markdown-header-face-5                        (:inherit 'markdown-header-face :height 0.90 :weight 'bold))
     (markdown-header-face-6                        (:inherit 'markdown-header-face :height 0.75 :weight 'extra-bold))

     ;; https://joaotavora.github.io/eglot/
     (eglot-diagnostic-tag-unnecessary-face         (:foreground sumiInk-4))
     ;; (eglot-type-hint-face)
     ;; (eglot-inlay-hint-face)
     ;; (eglot-parameter-hint-face)
     ;; (eglot-highlight-symbol-face)
     (eglot-diagnostic-tag-deprecated-face          (:foreground peachRed))

     ;; https://www.emacswiki.org/emacs/FlyMake
     (flymake-errline                               (:foreground peachRed))
     (flymake-error                                 (:foreground peachRed))
     (flymake-error-echo                            (:foreground peachRed))
     (flymake-note                                  (:foreground crystalBlue))
     (flymake-warline                               (:foreground roninYellow))
     (flymake-warning                               (:foreground roninYellow))

     ;; https://www.flycheck.org/en/latest
     (flycheck-error-list-error                     (:foreground samuraiRed :bold t))
     (flycheck-error-list-info                      (:foreground waveAqua1 :bold t))
     (flycheck-error-list-warning                   (:foreground roninYellow :bold t))
     (flycheck-fringe-error                         (:foreground samuraiRed))
     (flycheck-fringe-info                          (:foreground autumnGreen))
     (flycheck-fringe-warning                       (:foreground lightBlue))
     (flycheck-inline-error                         (:foreground samuraiRed :background winterRed :italic t :bold t :height 138))
     (flycheck-inline-info                          (:foreground lightBlue :background winterBlue :italic t  :bold t :height 138))
     (flycheck-inline-warning                       (:foreground winterYellow :background carpYellow :italic t :bold t :height 138))
     (flycheck-posframe-background-face             (:background sumiInk-0))
     (flycheck-posframe-error-face                  (:background sumiInk-0 :foreground samuraiRed))
     (flycheck-posframe-face                        (:background sumiInk-0))
     (flycheck-posframe-info-face                   (:background sumiInk-0 :foreground autumnGreen))
     (flycheck-posframe-warning-face                (:background sumiInk-0 :foreground lightBlue))

     ;; https://github.com/DarthFennec/highlight-indent-guides
     (highlight-indent-guides-character-face        (:foreground sumiInk-3))
     (highlight-indent-guides-even-face             (:foreground sumiInk-2))
     (highlight-indent-guides-odd-face              (:foreground comet))
     (highlight-indent-guides-stack-character-face  (:foreground sumiInk-3))
     (highlight-indent-guides-stack-character-face  (:foreground sumiInk-3))
     (highlight-indent-guides-stack-even-face       (:foreground comet))
     (highlight-indent-guides-stack-odd-face        (:foreground sumiInk-3))

     ;; https://lists.gnu.org/archive/html/help-gnu-emacs/2009-05/msg00013.html
     (highlight-numbers-face                        (:foreground sakuraPink))
     (highlight-operators-face                      (:foreground boatYellow2))
     (highlight-quoted-symbol                       (:foreground springGreen))
     (highlight-symbol-face                         (:background waveBlue-1 :foreground lightBlue))

     ;; https://oremacs.com/swiper
     (ivy-action                                    (:background nil :foreground fujiWhite))
     (ivy-confirm-face                              (:foreground waveAqua2))
     (ivy-current-match                             (:background crystalBlue :foreground sumiInk-0 :bold t))
     (ivy-grep-info                                 (:foreground lightBlue))
     (ivy-grep-line-number                          (:background nil :foreground springGreen))
     (ivy-grep-line-number                          (:foreground springViolet2))
     (ivy-minibuffer-match-face-1                   (:background nil :foreground waveRed))
     (ivy-minibuffer-match-face-2                   (:background nil :foreground springGreen))
     (ivy-minibuffer-match-highlight                (:foreground lightBlue))

     ;; https://github.com/tumashu/ivy-posframe
     (ivy-posframe                                  (:background sumiInk-2))
     (ivy-posframe-border                           (:background sumiInk-3))

     ;; https://www.emacswiki.org/emacs/IncrementalSearch
     (isearch                                       (:background waveBlue-2 :foreground carpYellow :italic t))
     (isearch-group-1                               (:background waveRed :foreground carpYellow :italic t))
     (isearch-group-2                               (:background springGreen :foreground carpYellow :italic t))

     ;; https://github.com/minad/vertico
     (vertico-current                               (:background waveBlue-1 :foreground carpYellow :distant-foreground fujiWhite :bold t))
     (vertico-group-separator                       (:background winterBlue :foreground lightBlue :strike-through t))
     (vertico-group-title                           (:background winterBlue :foreground lightBlue :weight 'medium :height 140))
     (vertico-multiline                             (:background samuraiRed))
     (vertico-mouse                                 (:background oniViolet :foreground fujiWhite))
     (vertico-posframe                              (:background sumiInk-2))
     (vertico-posframe-border                       (:background sumiInk-3))

     ;; https://github.com/minad/corfu
     (corfu-annotations                             (:foreground comet))
     (corfu-current                                 (:inherit 'vertico-current))
     (corfu-border                                  (:background carpYellow))
     (corfu-bar                                     (:background carpYellow))
     (corfu-default                                 (:background sumiInk-0 :foreground fujiWhite))
     (corfu-popupinfo                               (:background sumiInk-0 :foreground fujiWhite :italic t))

     ;; https://github.com/oantolin/orderless
     (orderless-match-face-0                        (:foreground waveAqua1 :weight 'bold :underline t))
     (orderless-match-face-1                        (:foreground springGreen :weight 'semi-bold))
     (orderless-match-face-2                        (:foreground autumnYellow :weight 'semi-bold))
     (orderless-match-face-3                        (:foreground sakuraPink :weight 'semi-bold))
     (comint-highlight-prompt                       (:background springViolet2 :foreground sumiInk-1))

     ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Completion-Variables.html
     (completions-annotations                       (:background nil :foreground dragonBlue :italic t))
     (completions-highlight                         (:foreground springViolet1 :italic t))
     (completions-common-part                       (:foreground boatYellow2 :distant-foreground sumiInk-1 :distant-background autumnGreen :bold t :italic t))
     (completions-first-difference                  (:foreground waveRed :strike-through t))

     ;; https://github.com/minad/consult
     (consult-file                                  (:foreground comet :distant-foreground sumiInk-1))

     ;; https://emacs-lsp.github.io/lsp-mode/page/settings/headerline/
     (lsp-headerline-breadcrumb-path-error-face     (:underline (:color springGreen :style 'wave) :foreground sumiInk-4 :background sumiInk-0))
     (lsp-headerline-breadcrumb-path-face           (:background sumiInk-0))
     (lsp-headerline-breadcrumb-path-hint-face      (:background sumiInk-0))
     (lsp-headerline-breadcrumb-path-info-face      (:background sumiInk-0))
     (lsp-headerline-breadcrumb-project-prefix-face (:background sumiInk-0))
     (lsp-headerline-breadcrumb-separator-face      (:background sumiInk-0))
     (lsp-headerline-breadcrumb-symbols-error-face  (:foreground peachRed))
     (lsp-headerline-breadcrumb-symbols-face        (:background sumiInk-0))

     ;; https://github.com/emacs-lsp/lsp-ui
     (lsp-ui-doc-background                         (:background sumiInk-0 :foreground peachRed))
     (lsp-ui-doc-border                             (:background nil :foreground nil))
     (lsp-ui-doc-header                             (:background sumiInk-0 :foreground peachRed))
     (lsp-ui-peek-filename                          (:foreground lightBlue))
     (lsp-ui-sideline-code-action                   (:foreground carpYellow))
     (lsp-ui-sideline-current-symbol                (:foreground springBlue))
     (lsp-ui-sideline-symbol                        (:foreground dragonBlue))

     ;; https://github.com/emacs-dashboard/emacs-dashboard
     (dashboard-banner-logo-title                   (:foreground boatYellow2 :height 200 :weight 'regular :italic t))
     (dashboard-footer-face                         (:foreground dragonBlue :height 135 :weight 'bold :italic t))
     (dashboard-heading                             (:foreground springViolet2 :height 150 :weight 'regular))
     (dashboard-items-face                          (:weight 'semi-bold))
     (dashboard-navigator                           (:foreground waveAqua1 :weight 'semi-bold :italic nil))
     (dashboard-no-items-face                       (:foreground sumiInk-4))

     ;; https://github.com/domtronn/all-the-icons.el
     (all-the-icons-dgreen                          (:foreground waveAqua2))
     (all-the-icons-green                           (:foreground waveAqua2))
     (all-the-icons-dpurple                         (:foreground springViolet2))
     (all-the-icons-purple                          (:foreground springViolet2))

     ;; https://github.com/Alexander-Miller/treemacs
     (treemacs-directory-collapsed-face             (:foreground fujiWhite))
     (treemacs-directory-face                       (:foreground fujiWhite))
     (treemacs-file-face                            (:foreground fujiWhite))

     (treemacs-git-added-face                       (:foreground surimiOrange))
     (treemacs-git-ignored-face                     (:foreground sumiInk-4))
     (treemacs-git-modified-face                    (:foreground springGreen))
     (treemacs-git-renamed-face                     (:foreground fujiWhite))
     (treemacs-git-renamed-face                     (:foreground fujiWhite))
     (treemacs-git-unmodified-face                  (:foreground fujiWhite))

     ;; https://github.com/hlissner/emacs-solaire-mode
     (solaire-default-face                          (:background sumiInk-1))

     ;; https://www.emacswiki.org/emacs/EdiffMode
     (diff-added                                    (:background autumnGreen :foreground fujiWhite))
     (diff-changed                                  (:background autumnYellow :foreground sumiInk-1))

     ;; (related) https://github.com/emacs-evil/evil
     (evil-ex-search                                (:foreground winterBlue :background dragonBlue :bold t))
     (evil-ex-lazy-highlight                        (:inherit 'evil-ex-search))
     (evil-ex-substitute-matches                    (:foreground winterRed  :background autumnRed :bold t))
     (evil-ex-substitute-replacement                (:foreground winterGreen :background autumnGreen))

     ;; https://github.com/blorbx/evil-quickscope
     (evil-quickscope-first-face                    (:foreground autumnYellow :underline t))
     (evil-quickscope-second-face                   (:foreground surimiOrange :underline t))

     ;; https://github.com/edkolev/evil-goggles
     (evil-goggles-default-face                     (:background waveBlue-2))
     (evil-goggles-join-face                        (:background waveBlue-2))
     (evil-goggles-delete-face                      (:background peachRed))
     (evil-goggles-paste-face                       (:background autumnYellow :foreground sumiInk-1))
     (evil-goggles-indent-face                      (:background springGreen))
     (evil-goggles-set-marker-face                  (:background waveRed :foreground peachRed))
     (evil-goggles-yank-face                        (:background autumnGreen :foreground sumiInk-1))

     ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Term-Mode.html
     (term                                          (:background sumiInk-0 :foreground fujiWhite))
     (term-color-blue                               (:background crystalBlue :foreground crystalBlue))
     (term-color-bright-blue                        (:inherit 'term-color-blue))
     (term-color-green                              (:background waveAqua2 :foreground waveAqua2))
     (term-color-bright-green                       (:inherit 'term-color-green))
     (term-color-black                              (:background sumiInk-0 :foreground fujiWhite))
     (term-color-bright-black                       (:background sumiInk-1b :foreground sumiInk-1b))
     (term-color-white                              (:background fujiWhite :foreground fujiWhite))
     (term-color-bright-white                       (:background old-white :foreground old-white))
     (term-color-red                                (:background peachRed :foreground peachRed))
     (term-color-bright-red                         (:background springGreen :foreground springGreen))
     (term-color-yellow                             (:background carpYellow :foreground carpYellow))
     (term-color-bright-yellow                      (:background carpYellow :foreground carpYellow))
     (term-color-cyan                               (:background springBlue :foreground springBlue))
     (term-color-bright-cyan                        (:background springBlue :foreground springBlue))
     (term-color-magenta                            (:background springViolet2 :foreground springViolet2))
     (term-color-bright-magenta                     (:background springViolet2 :foreground springViolet2))

     ;; https://github.com/akermu/emacs-libvterm
     (vterm-color-blue                              (:background crystalBlue :foreground crystalBlue))
     (vterm-color-cyan                              (:background springBlue :foreground springBlue))
     (vterm-color-green                             (:background waveAqua2 :foreground waveAqua2))
     (vterm-color-magenta                           (:background springViolet2 :foreground springViolet2))
     (vterm-color-yellow                            (:background carpYellow :foreground carpYellow))
     (vterm-color-red                               (:background peachRed :foreground peachRed))
     (vterm-color-crust                             (:background sumiInk-1 :foreground sumiInk-1))
     (vterm-color-rosewater                         (:background fujiWhite :foreground fujiWhite))

     ;; https://www.emacswiki.org/emacs/AnsiColor
     (ansi-color-green                              (:foreground springGreen))
     (ansi-color-black                              (:background sumiInk-0))
     (ansi-color-cyan                               (:foreground waveAqua2))
     (ansi-color-magenta                            (:foreground sakuraPink))
     (ansi-color-blue                               (:foreground crystalBlue))
     (ansi-color-red                                (:foreground peachRed))
     (ansi-color-white                              (:foreground fujiWhite))
     (ansi-color-yellow                             (:foreground autumnYellow))
     (ansi-color-bright-white                       (:foreground old-white))

     ;; https://github.com/auto-complete/popup-el
     (popup-face                                    (:inherit 'tooltip))
     (popup-tip-face                                (:inherit 'tooltip))
     (popup-selection-face                          (:inherit 'tooltip))

     ;; https://github.com/emacsorphanage/anzu
     (anzu-match-1                                  (:foreground waveAqua2 :background sumiInk-2))
     (anzu-match-2                                  (:foreground carpYellow :background sumiInk-2))
     (anzu-match-3                                  (:foreground lightBlue :background sumiInk-2))

     (anzu-mode-line                                (:foreground sumiInk-0 :background springViolet2))
     (anzu-mode-no-match                            (:foreground fujiWhite :background peachRed))
     (anzu-replace-to                               (:foreground springBlue :background winterBlue))
     (anzu-replace-highlight                        (:foreground peachRed :background winterRed :strike-through t))

     ;; https://github.com/winterTTr/ace-jump-mode
     (ace-jump-face-background                      (:foreground waveBlue-2))
     (ace-jump-face-foreground                      (:foreground peachRed :background sumiInk-0 :bold t))

     ;; https://github.com/abo-abo/hydra
     (hydra-face-amaranth                           (:foreground autumnRed))
     (hydra-face-blue                               (:foreground springBlue))
     (hydra-face-pink                               (:foreground sakuraPink))
     (hydra-face-red                                (:foreground peachRed))
     (hydra-face-teal                               (:foreground lightBlue))

     ;; https://www.emacswiki.org/emacs/TabBarMode
     (tab-bar                                       (:background sumiInk-0  :foreground sumiInk-4))
     (tab-bar-tab                                   (:background sumiInk-1b :foreground waveAqua1 :bold t))
     (tab-bar-tab-inactive                          (:background sumiInk-0  :foreground sumiInk-4))

     ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Tab-Line.html
     (tab-line                                      (:background sumiInk-0 :foreground sumiInk-4))
     (tab-line-tab                                  (:background sumiInk-0  :foreground sumiInk-4))
     (tab-line-tab-current                          (:background sumiInk-1b :foreground fujiWhite :bold t))
     (tab-line-tab-inactive                         (:background sumiInk-0  :foreground sumiInk-4))
     (tab-line-tab-modified                         (:foreground autumnYellow))
     (tab-line-tab-special                          (:foreground springViolet1))
     (tab-line-tab-inactive-alternate               (:background sumiInk-0  :foreground sumiInk-4))
     (tab-line-highlight                            (:background sumiInk-1b :foreground boatYellow2 :bold t))
     (tab-line-close-highlight                      (:background sumiInk-0  :foreground peachRed))
     (tab-line-tab-group                            (:background sumiInk-0  :foreground waveAqua1 :bold t))

     ;; https://github.com/ema2159/centaur-tabs
     (centaur-tabs-default                          (:background sumiInk-0 :foreground sumiInk-4))
     (centaur-tabs-active-bar-face                  (:background carpYellow :foreground fujiWhite))
     (centaur-tabs-selected                         (:background sumiInk-1b :foreground fujiWhite))
     (centaur-tabs-selected-modified                (:background sumiInk-1b :foreground fujiWhite))
     (centaur-tabs-modified-marker-selected         (:background sumiInk-1b :foreground autumnYellow))
     (centaur-tabs-close-selected                   (:inherit 'centaur-tabs-selected))
     (centaur-tabs-unselected                       (:background sumiInk-0 :foreground sumiInk-4))
     (centaur-tabs-unselected-modified              (:background sumiInk-0 :foreground peachRed))
     (centaur-tabs-modified-marker-unselected       (:background sumiInk-0 :foreground sumiInk-4))
     (centaur-tabs-close-unselected                 (:background sumiInk-0 :foreground sumiInk-4))
     (centaur-tabs-close-mouse-face                 (:background nil :foreground peachRed))
     (centaur-tabs-name-mouse-face                  (:foreground springBlue :bold t))

     ;; https://github.com/Artawower/blamer.el
     (blamer-face                                   (:foreground dragonBlue :italic t))
     (blamer-pretty-border-face                     (:foreground dragonBlue))
     (blamer-pretty-meta-data-face                  (:foreground carpYellow))
     (blamer-pretty-meta-keywords-face              (:foreground autumnRed))
     (blamer-pretty-commit-message-face             (:foreground autumnGreen :italic t))

     ;; https://github.com/magit/magit
     (magit-section-heading                         (:foreground waveAqua2))
     (magit-section-highlight                       (:background sumiInk-1))
     (magit-diff-context-highlight                  (:inherit 'magit-diff-context-highlight))

     ;; https://github.com/emacsorphanage/git-gutter
     (git-gutter:added                              (:foreground autumnGreen))
     (git-gutter:deleted                            (:foreground waveRed))
     (git-gutter:modified                           (:foreground springBlue))

     ;; https://www.emacswiki.org/emacs/EdiffMode
     (diff-hl-margin-change                         (:foreground springBlue :background winterBlue))
     (diff-hl-margin-delete                         (:foreground peachRed :background winterRed))
     (diff-hl-margin-insert                         (:foreground comet :background winterBlue))

     ;; https://github.com/minad/goggles
     (goggles-added                                 (:background autumnGreen))
     (goggles-changed                               (:background autumnYellow))
     (goggles-removed                               (:background waveRed))

     ;; https://github.com/joodland/bm
     (bm-fringe-face                                (:background peachRed :foreground sumiInk-3))
     (bm-fringe-persistent-face                     (:background peachRed :foreground sumiInk-3))

     ;; https://tree-sitter.github.io/tree-sitter
     (tree-sitter-hl-face:attribute                 (:foreground surimiOrange))
     (tree-sitter-hl-face:escape                    (:foreground waveRed))
     (tree-sitter-hl-face:constructor               (:foreground waveRed :weight 'semi-bold))

     (tree-sitter-hl-face:constant                  (:foreground surimiOrange))
     (tree-sitter-hl-face:constant.builtin          (:foreground carpYellow :weight 'semi-bold))

     (tree-sitter-hl-face:embedded                  (:foreground boatYellow2))

     (tree-sitter-hl-face:function                  (:foreground crystalBlue))
     (tree-sitter-hl-face:function.builtin          (:foreground peachRed :italic t :background winterRed))
     (tree-sitter-hl-face:function.call             (:foreground springViolet2))
     (tree-sitter-hl-face:function.macro            (:foreground samuraiRed))
     (tree-sitter-hl-face:function.special          (:foreground sakuraPink))
     (tree-sitter-hl-face:function.label            (:foreground surimiOrange))

     (tree-sitter-hl-face:method                    (:foreground lightBlue))
     (tree-sitter-hl-face:method.call               (:foreground lightBlue))

     (tree-sitter-hl-face:property                  (:foreground carpYellow))
     (tree-sitter-hl-face:property.definition       (:foreground old-white :italic t))

     (tree-sitter-hl-face:tag                       (:foreground peachRed))

     (tree-sitter-hl-face:type                      (:foreground waveAqua2 :weight 'semi-bold))
     (tree-sitter-hl-face:type.argument             (:foreground surimiOrange))
     (tree-sitter-hl-face:type.builtin              (:foreground autumnRed))
     (tree-sitter-hl-face:type.parameter            (:foreground surimiOrange))
     (tree-sitter-hl-face:type.super                (:foreground samuraiRed :bold t))

     (tree-sitter-hl-face:variable                  (:foreground springBlue :italic t))
     (tree-sitter-hl-face:variable.builtin          (:foreground waveRed))
     (tree-sitter-hl-face:variable.parameter        (:foreground springViolet2 :italic t))
     (tree-sitter-hl-face:variable.special          (:foreground surimiOrange))
     (tree-sitter-hl-face:variable.synthesized      (:foreground lightBlue))

     (tree-sitter-hl-face:number                    (:foreground sakuraPink))
     (tree-sitter-hl-face:operator                  (:foreground sakuraPink :bold t))

     (tree-sitter-hl-face:punctuation               (:foreground lightBlue))
     (tree-sitter-hl-face:punctuation.bracket       (:foreground springViolet2 :bold t))
     (tree-sitter-hl-face:punctuation.delimiter     (:foreground springViolet2 :bold t))
     (tree-sitter-hl-face:punctuation.special       (:foreground peachRed))

     (tree-sitter-hl-face:case-pattern              (:foreground waveRed))
     (tree-sitter-hl-face:variable.synthesized      (:foreground waveRed))
     (tree-sitter-hl-face:keyword.compiler          (:foreground peachRed :bold t :italic t))

     (focus-unfocused (:foreground sumiInk-4)))

    ,@body))

(provide 'kanagawa)
;;; kanagawa.el ends here
