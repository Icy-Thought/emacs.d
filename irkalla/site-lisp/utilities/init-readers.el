;;; init-reader.el --- Readers: Emacs Document Readers -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; Several document readers (PDF,EPUB,ETC.) for the mind that likes to read.

;;; Code:

;; :NOTE| PDF-Viewer for readers out there!
(use-package pdf-tools
  :elpaca nil ;; <-^ fetched from Nixpkgs
  :init (push 'pdf-tools elpaca-ignored-dependencies)) ;; Allows proper detection of ~epdfinfo~

(use-package pdf-view
  :elpaca nil
  :magic ("%PDF" . pdf-view-mode)
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :hook (pdf-view-mode . (lambda ()
                           ;; :NOTE| Set the PDF free from the unnecessary borders
                           (set (make-local-variable 'evil-normal-state-cursor) (list nil))
                           (pdf-view-themed-minor-mode)))
  :config
  (define-advice pdf-view-enlarge (:after (&rest _args) center-after-enlarge)
    "Center the PDF view in the active PDF window after enlarging it."
   (pdf-view-center-in-window))

  (define-advice pdf-view-shrink (:after (&rest _args) center-after-shrink)
    "Center the PDF view in the active PDF window after shrinking it."
    (pdf-view-center-in-window))
  :custom
  (pdf-view-use-scaling t)
  (pdf-view-use-imagemagick nil)
  (pdf-view-display-size 'fit-width))

(use-package pdf-view-restore
  :hook (pdf-view-mode . pdf-view-restore-mode)
  :custom (pdf-view-restore-filename (no-littering-expand-var-file-name "pdf-view-restore")))

;; :NOTE| A Customizable EPUB Reader
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :hook (nov-mode . olivetti-mode)
  :general (:states 'normal :keymaps 'nov-mode-map
             "H"   '(nov-previous-document   :which-key "Go -> previous doc")
             "L"   '(nov-previous-document   :which-key "Go -> next doc")
             "d"   '(nov-scroll-down         :which-key "Scroll downwards")
             "u"   '(nov-scroll-up           :which-key "Scroll upwards")
             "gm"  '(nov-display-metadata    :which-key "Show Metadata")
             "gr"  '(nov-render-document     :which-key "Render document")
             "o"   '(nov-goto-toc            :which-key "Table of contents")
             "gv"  '(nov-view-source         :which-key "View source")
             "gV"  '(nov-view-content-source :which-key "View content source"))
  :custom-face
  (shr-text ((t (:inherit variable-pitch-face :height 1.05))))
  (shr-h1   ((t (:height 1.54 :slant italic))))
  (shr-h2   ((t (:height 1.25 :slant italic))))
  (shr-h3   ((t (:height 1.15 :slant italic))))
  (shr-h4   ((t (:height 1.12 :slant italic))))
  (shr-h5   ((t (:height 1.09 :slant italic))))
  (shr-h6   ((t (:height 1.06 :slant italic))))
  :custom (nov-text-width t))

(use-package nov-xwidget
  :elpaca (:host github :repo "chenyanming/nov-xwidget")
  :hook (nov-mode . nov-xwidget-inject-all-files)
  :general (:states 'normal :keymaps 'nov-mode-map
             "x" '(nov-xwidget-view :which-key "Open EPUB -> Nov-Mode")))

;; :NOTE| A RSS-reader for our curious minds
(use-package newsticker
  :elpaca nil
  :preface 
  (defun irkalla/newsticker-start-newTab ()
    "Launch NewsTicker (TreeView) in a new tab."
    (interactive)
    (let (success)
      (unwind-protect
          (progn
            (tab-bar-new-tab)
            (call-interactively #'newsticker-treeview)
            (tab-bar-rename-tab "newsticker")
            (setq success t))
        (unless success
          (tab-bar-close-tab)))))

  (defun irkalla/newsticker-quit-newTab ()
    "Quit NewsTicker (TreeView) -> stop NewsTicker -> close tab."
    (interactive)
    (newsticker-treeview-quit)
    (newsticker-stop)
    (tab-close))
  :hook (newsticker-treeview-item-mode . olivetti-mode)
  :general
  (:states 'normal :keymaps 'newsticker-treeview-mode-map
    "o" 'newsticker-treeview-browse-url
    "q" 'irkalla/newsticker-quit-newTab)

  (irkalla/comma-lead-keydef
    "r"   '(:ignore t                       :which-key "RSS Reader")
    "r o" '(irkalla/newsticker-start-newTab :which-key "Open Treeview"))
  :custom
  (newsticker-automatically-mark-items-as-old nil)
  (newsticker-automatically-mark-visited-items-as-old t)
  (newsticker-obsolete-item-max-age 259200) ;; 3 days
  (newsticker-retrieval-method 'extern)
  (newsticker-treeview-automatically-mark-displayed-items-as-old nil)

  (newsticker-url-list-defaults nil)
  (newsticker-url-list
   '(("Planet Emacslife"            "https://planet.emacslife.com/atom.xml")
     ("Sacha Chua"                  "https://sachachua.com/blog/feed/")
     ("Mastering Emacs"             "http://www.masteringemacs.org/feed/")
     ;; ---[ Science & Technology ]---
     ("Phys.org: Physics"           "https://phys.org/rss-feed/")
     ("Quanta Magazine"             "https://api.quantamagazine.org/feed/")
     ;; ---[ Mathematics ]---
     ("Arxiv: Mathematics"          "http://arxiv.org/rss/math")
     ("Arxiv: Mathematical Physics" "http://arxiv.org/rss/math-ph")
     ("Terrence Tao (Blog)"         "https://terrytao.wordpress.com/feed/")
     ("Stephen Wolfram (Blog)"      "https://writings.stephenwolfram.com/feed/")
     ;; ---[ Computer Science ]---
     ("Arxiv: Computer Science"     "http://arxiv.org/rss/cs")
     ;; ---[ Physics ]---
     ("Arxiv: Physics"              "http://arxiv.org/rss/physics")))

  (newsticker-wget-arguments '("--silent" "--location" "--connect-timeout" "8"))
  (newsticker-wget-name "curl"))

(provide 'init-readers)
;;; init-readers.el ends here
