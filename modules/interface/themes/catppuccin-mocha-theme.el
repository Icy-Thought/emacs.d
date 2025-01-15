;;; package: --- Catppuccin Mocha: dark colorscheme -*- lexical-binding: t;; -*-

;;; Commentary:
;;; [1]: https://github.com/NamesCode/.Dotfiles/blob/9b702d91df512b94993f4db8b53acbf447e6fe27/ext/doom/themes/catppuccin-mocha-theme.el#L15

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
 catppuccin-mocha
 "The darker side of Catppuccin"
 ((((class color) (min-colors #xFFFFFF))        ; col 1 GUI/24bit
   ((class color) (min-colors #xFF)))           ; col 2 Xterm/256

  (rosewater  "#f5e0dc" "#ffffff")
  (flamingo   "#f2cdcd" "#ffd7df")
  (pink       "#f5c2e7" "#d7afaf")
  (mauve      "#cba6f7" "#d7afd7")
  (red        "#f38ba8" "#ff87af")
  (maroon     "#eba0ac" "#ffafaf")
  (peach      "#fab387" "#ffaf87")
  (yellow     "#f9e2af" "#ffd7af")
  (green      "#a6e3a1" "#87afaf")
  (teal       "#94e2d5" "#afd7d7")
  (sky        "#89dceb" "#afffff")
  (sapphire   "#74c7ec" "#afffff")
  (blue       "#89b4fa" "#00d7ff")
  (lavender   "#b4befe" "#d7d7ff")
  (text       "#cdd6f4" "#ffffff")
  (subtext1   "#bac2de" "#ffffff")
  (subtext0   "#a6adc8" "#ffffff")
  (overlay2   "#9399b2" "#ffffff")
  (overlay1   "#7f849c" "#ffffff")
  (overlay0   "#6c7086" "#ffffff")
  (surface2   "#585b70" "#ffffff")
  (surface1   "#45475a" "#ffffff")
  (surface0   "#313244" "#ffffff")
  (base       "#1e1e2e" "#ffffff")
  (mantle     "#181825" "#ffffff")
  (crust      "#11111b" "#ffffff")))

(provide-theme 'catppuccin-mocha)
;;; catppuccin-mocha-theme.el ends here
