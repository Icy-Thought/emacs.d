;;; package: --- A theme inspired by the colors of the famous painting by Katsushika Hokusa! -*- lexical-binding: t; -*-

;;; Commentary:
;;; [1]: https://github.com/rebelot/kanagawa.nvim
;;; [2]: https://github.com/konrad1977/kanagawa-emacs

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

(require 'kanagawa)

(kanagawa-deftheme
 kanagawa-wave
 "A theme inspired by the colors of the famous painting by Katsushika Hokusa"
 ((((class color) (min-colors #xFFFFFF))        ; col 1 GUI/24bit
   ((class color) (min-colors #xFF)))           ; col 2 Xterm/256

  ;; Define our color palette
  (fujiWhite     "#DCD7BA" "#FFFFFF")
  (old-white     "#C8C093" "#FFFFFF")

  (sumiInk-0     "#16161D" "#000000")
  (sumiInk-1b    "#181820" "#000000")
  (sumiInk-1     "#1F1F28" "#080808")
  (sumiInk-2     "#2A2A37" "#121212")
  (sumiInk-3     "#363646" "#303030")
  (sumiInk-4     "#54546D" "#303030")

  (waveBlue-1    "#223249" "#4E4E4E")
  (waveBlue-2    "#2D4F67" "#585858")
  (waveAqua1     "#6A9589" "#6A9589")
  (waveAqua2     "#7AA89F" "#717C7C")

  (winterGreen   "#2B3328" "#585858")
  (winterYellow  "#49443C" "#585858")
  (winterRed     "#43242B" "#585858")
  (winterBlue    "#252535" "#585858")

  (autumnGreen   "#76946A" "#585858")
  (autumnRed     "#C34043" "#585858")
  (autumnYellow  "#DCA561" "#585858")

  (samuraiRed    "#E82424" "#585858")
  (roninYellow   "#FF9E3B" "#585858")

  (dragonBlue    "#658594" "#658594")
  (fujiGray      "#727169" "#717C7C")
  (springViolet1 "#938AA9" "#717C7C")
  (oniViolet     "#957FB8" "#717C7C")
  (crystalBlue   "#7E9CD8" "#717C7C")
  (springViolet2 "#9CABCA" "#717C7C")
  (springBlue    "#7FB4CA" "#717C7C")
  (lightBlue     "#A3D4D5" "#717C7C")
  (springGreen   "#98BB6C" "#717C7C")
  (boatYellow1   "#938056" "#717C7C")
  (boatYellow2   "#C0A36E" "#717C7C")
  (carpYellow    "#E6C384" "#717C7C")
  (sakuraPink    "#D27E99" "#717C7C")
  (waveRed       "#E46876" "#717C7C")
  (peachRed      "#FF5D62" "#717C7C")
  (surimiOrange  "#FFA066" "#717C7C")
  (katanaGray    "#717C7C" "#717C7C")
  (comet         "#54536D" "#4e4e4e")))

(provide-theme 'kanagawa-wave)
;;; kanagawa-wave-theme.el ends here
