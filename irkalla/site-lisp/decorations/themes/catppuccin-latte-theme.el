;;; package: --- Catppuccin Latte: bright colorscheme -*- lexical-binding: t;; -*-

;;; Commentary:
;;; [1]: https://github.com/catppuccin/emacs/blob/main/catppuccin-theme.el

;;; Code:
(eval-when-compile
  (require 'cl-lib))

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(require 'catppuccin)

(catppuccin-deftheme
 catppuccin-latte
 "The brighter side of Catppuccin"
 ((((class color) (min-colors #xFFFFFF))        ; col 1 GUI/24bit
   ((class color) (min-colors #xFF)))           ; col 2 Xterm/256

  (rosewater  "#dc8a78" "#ffffff")
  (flamingo   "#dd7878" "#ffd7df")
  (pink       "#ea76cb" "#d7afaf")
  (mauve      "#8839ef" "#d7afd7")
  (red        "#d20f39" "#ff87af")
  (dark-red   "#311c22" "#311c22")
  (maroon     "#e64553" "#ffafaf")
  (peach      "#fe640b" "#ffaf87")
  (yellow     "#df8e1d" "#ffd7af")
  (green      "#40a02b" "#87afaf")
  (teal       "#179299" "#afd7d7")
  (sky        "#04a5e5" "#afffff")
  (sapphire   "#209fb5" "#afffff")
  (blue       "#1e66f5" "#00d7ff")
  (lavender   "#7287fd" "#d7d7ff")
  (text       "#4c4f69" "#ffffff")
  (subtext1   "#5c5f77" "#ffffff")
  (subtext0   "#6c6f85" "#ffffff")
  (overlay2   "#7c7f93" "#ffffff")
  (overlay1   "#8c8fa1" "#ffffff")
  (overlay0   "#9ca0b0" "#ffffff")
  (surface2   "#acb0be" "#ffffff")
  (surface1   "#bcc0cc" "#ffffff")
  (surface0   "#ccd0da" "#ffffff")
  (base       "#eff1f5" "#ffffff")
  (mantle     "#e6e9ef" "#ffffff")
  (crust      "#dce0e8" "#ffffff")))

(provide-theme 'catppuccin-latte)
;;; catppuccin-latte-theme.el ends here
