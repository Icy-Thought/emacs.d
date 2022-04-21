;; Author: splpaul (github) ++ customizations by myself.
;; -*- lexical-binding: t; no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-catppuccin-theme nil
  "Options for the `doom-cattpuccin' theme."
  :group 'doom-themes)

(defcustom doom-catppuccin-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-catppuccin-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-catppuccin
  "A dark theme inspired by Catppuccin"

  ;; name        default   256       16
  ((bg         '("#1A1826" nil       nil))
   (bg-alt     '("#1E1D2F" nil       nil))
   (base0      '("#161320" "black"   "black"))
   (base1      '("#1A1826" "#262626" "brightblack"))
   (base2      '("#1A1826" "#303030" "brightblack"))
   (base3      '("#1A1826" "#3a3a3a" "brightblack"))
   (base4      '("#302D41" "#444444" "brightblack"))
   (base5      '("#676E95" "#585858" "brightblack"))
   (base6      '("#697098" "#626262" "brightblack"))
   (base7      '("#717CB4" "#767676" "brightblack"))
   (base8      '("#D9E0EE" "#a8a8a8" "white"))
   (fg         '("#EEFFFF" "#e4e4e4" "brightwhite"))
   (fg-alt     '("#BFC7D5" "#bcbcbc" "white"))

   (grey base5)

   (magenta     '("#DDB6F2" "#DDB6F2" "brightmagenta"))
   (violet      '("#F5C2E7" "#F5C2E7" "magenta"))
   (red         '("#F28FAD" "#F28FAD" "red"))
   (orange      '("#F8BD96" "#ff5f00" "brightred"))
   (yellow      '("#FAE3B0" "#ffd700" "brightyellow"))
   (green       '("#ABE9B3" "#afff00" "green"))
   (teal        '("#B5E8E0" "#00d7af" "brightgreen"))
   (blue        '("#89DCEB" "#5fafff" "brightblue"))
   (dark-blue   '("#96CDFB" "#d7ffff" "blue"))
   (cyan        '("#C9CBFF" "#5fd7ff" "brightcyan"))
   (dark-cyan   '("#C9CBFF" "#00d7af" "cyan"))

   ;; face categories -- required for all themes
   (highlight      magenta)
   (vertical-bar   base2)
   (selection      base4)
   (builtin        blue)
   (comments       base5)
   (doc-comments   (doom-lighten base5 0.25))
   (constants      orange)
   (functions      blue)
   (keywords       cyan)
   (methods        blue)
   (operators      cyan)
   (type           magenta)
   (strings        green)
   (variables      yellow)
   (numbers        orange)
   (region         base4)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    blue)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (modeline-bg     base2)
   (modeline-bg-alt (doom-darken bg 0.01))
   (modeline-fg     base8)
   (modeline-fg-alt comments)

   (-modeline-pad
    (when doom-catppuccin-padded-modeline
      (if (integerp doom-catppuccin-padded-modeline) doom-catppuccin-padded-modeline 4))))

  ;;;; Base theme face overrides
  ((lazy-highlight :background base4 :foreground fg :weight 'bold)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (tooltip :background (doom-darken bg-alt 0.2) :foreground fg)

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; dired-k
   (dired-k-commited :foreground base4)
   (dired-k-modified :foreground vc-modified)
   (dired-k-ignored :foreground cyan)
   (dired-k-added    :foreground vc-added)
   ;;;; doom-modeline
   (doom-modeline-buffer-path       :foreground green :weight 'bold)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path)
   ;;;; js2-mode
   (js2-jsdoc-tag              :foreground magenta)
   (js2-object-property        :foreground yellow)
   (js2-object-property-access :foreground cyan)
   (js2-function-param         :foreground violet)
   (js2-jsdoc-type             :foreground base8)
   (js2-jsdoc-value            :foreground cyan)
   ;;;; man <built-in>
   (Man-overstrike :inherit 'bold :foreground magenta)
   (Man-underline :inherit 'underline :foreground blue)
   ;;;; org <built-in>
   ((org-block &override) :background base2)
   ((org-block-background &override) :background base2)
   ((org-block-begin-line &override) :background base2)
   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground magenta)
   (rainbow-delimiters-depth-2-face :foreground orange)
   (rainbow-delimiters-depth-3-face :foreground green)
   (rainbow-delimiters-depth-4-face :foreground cyan)
   (rainbow-delimiters-depth-5-face :foreground violet)
   (rainbow-delimiters-depth-6-face :foreground yellow)
   (rainbow-delimiters-depth-7-face :foreground blue)
   (rainbow-delimiters-depth-8-face :foreground teal)
   (rainbow-delimiters-depth-9-face :foreground dark-cyan)
   ;;;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground yellow :slant 'italic :weight 'medium)))

;;; doom-catppuccin-theme.el ends here
