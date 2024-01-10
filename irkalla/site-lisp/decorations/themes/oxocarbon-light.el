;;; oxocarbon-light.el --- All natural pine, faux fur and a bit of soho vibes! -*- lexical-binding: t; -*-

;;; Commentary
;;; [1]: https://github.com/nyoom-engineering/oxocarbon

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

(require 'oxocarbon)

(oxocarbon-deftheme
 oxocarbon-light
 "The brighter side of Oxocarbon"
 ((((class color) (min-colors #xFFFFFF))        ; col 1 GUI/24bit
   ((class color) (min-colors #xFF)))           ; col 2 Xterm/256

  ;; Define our color palette
  (highlight-high   "#464646" "#d7d7ff")
  (highlight-med    "#363636" "#d7d7ff")
  (highlight-low    "#272727" "#d7d7ff")
  (iris             "#be95ff" "#d7d7ff")
  (dark-iris        "#673ab7" "#d7d7ff")
  (foam             "#0f62fe" "#00d7ff")
  (dark-foam        "#142027" "#00d7ff")
  (medium-foam      "#434d52" "#00d7ff")
  (pine             "#08bdba" "#afffff")
  (rose             "#f5e0dc" "#ffffff")
  (gold             "#ff6f00" "#ffd7af")
  (dark-gold        "#211A10" "#ffd7af")
  (medium-gold      "#624d30" "#ffd7af")
  (pink             "#ff7eb6" "#ffd7af")
  (love             "#ee5396" "#ff87af")
  (dark-love        "#27000f" "#ff87af")
  (medium-love      "#53333f" "#ff87af")
  (green            "#42be65" "#00d7ff")
  (dark-green       "#03302f" "#f00000")
  (medium-green     "#365958" "#f00000")
  (text             "#161616" "#ffffff")
  (subtle           "#dde1e6" "#ffffff")
  (unmuted          "#bbc1c6" "#686868")
  (muted            "#525252" "#ffffff")
  (overlay          "#262626" "#ffffff")
  (surface          "#f2f4f8" "#ffffff")
  (base             "#131313" "#ffffff")
  (crust            "#050505" "#ffffff")))

(provide-theme 'oxocarbon-light)
;;; oxocarbon-light-theme.el ends here
