;;; rose-pine.el --- All natural pine, faux fur and a bit of soho vibes! -*- lexical-binding: t; -*-

;;; Commentary
;;; [1]: https://rosepinetheme.com/
;;; [2]: https://github.com/articuluxe/harmsway/blob/master/.emacs.d/etc/themes/rose-pine-theme.el

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

(require 'rose-pine)

(rose-pine-deftheme
 rose-pine-dark
 "All natural pine, faux fur and a bit of soho vibes for the classy minimalist."

 ((((class color) (min-colors #xFFFFFF))        ; col 1 GUI/24bit
   ((class color) (min-colors #xFF)))           ; col 2 Xterm/256

  ;; Define our color palette
  (highlight-high   "#524f67" "#d7d7ff")
  (highlight-med    "#403d52" "#d7d7ff")
  (highlight-low    "#21202E" "#d7d7ff")
  (iris             "#c4a7e7" "#d7d7ff")
  (foam             "#9ccfd8" "#00d7ff")
  (foam-low         "#272B37" "#00d7ff")
  (pine             "#31748f" "#afffff")
  (rose             "#ebbcba" "#ffffff")
  (rose-low         "#302935" "#ffffff")
  (gold             "#f6c177" "#ffd7af")
  (gold-low         "#312A2E" "#ffd7af")
  (love             "#eb6f92" "#ff87af")
  (text             "#e0def4" "#ffffff")
  (subtle           "#908caa" "#ffffff")
  (muted            "#6e6a86" "#ffffff")
  (overlay          "#26233a" "#ffffff")
  (surface          "#1F1D2E" "#ffffff")
  (base             "#191724" "#ffffff")
  (ex               "#16141f" "#F00000")))

(provide-theme 'rose-pine-dark)
;;; rose-pine-dark-theme.el ends here
