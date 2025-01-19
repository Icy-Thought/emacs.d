;;; my-readers.el --- Various Document Readers -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-feature pdf-tools
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :hook (pdf-view-mode . pdf-view-midnight-minor-mode)
  :bind (:map pdf-view-mode-map ([tab] . pdf-outline))
  :config
  ;; :NOTE| Set the PDF free from the unnecessary borders.
  (when (featurep 'evil)
    (add-hook 'pdf-view-mode-hook
              (lambda () (set (make-local-variable 'evil-normal-state-cursor)
                         (list nil)))))

  ;; :NOTE| Load PDF-Tools utilities when installed from Nix || Guix.
  (if IS-DECLERATIVE? (let ((inhibit-message t))
                        (load-library "pdf-tools-autoloads")))

  ;; :NOTE|Auto center PDF page on zoom-in/out.
  (advice-add 'pdf-view-enlarge :after (lambda (&rest _args) (pdf-view-center-in-window)))
  (advice-add 'pdf-view-shrink :after (lambda (&rest _args) (pdf-view-center-in-window)))
  :custom
  (pdf-view-use-scaling t)
  (pdf-view-use-imagemagick nil)
  (pdf-view-display-size 'fit-width)
  (pdf-view-midnight-colors '("#DCD7BA" . "#16161D")))

;; :NOTE| Restore last known position of the PDF document

(use-package pdf-view-restore
  :hook (pdf-view-mode . pdf-view-restore-mode)
  :custom (pdf-view-restore-filename (no-littering-expand-var-file-name "pdf-view-restore")))

;; :NOTE| .epub reader

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (when (fboundp 'visual-fill-column-mode)
    (add-hook 'nov-mode-hook #'visual-fill-column-mode))
  :custom (nov-text-width t))

;; :NOTE| Render .epub files in xwidgets -> cleaner!

(use-package nov-xwidget
  :vc (:url "https://github.com/chenyanming/nov-xwidget")
  :if (featurep 'xiwdget-internal)
  :hook (nov-mode . nov-xwidget-inject-all-files)
  :bind (:map nov-mode-map ("o" . #'nov-xwdiget-view)))

;; :NOTE| RSS-feed reader

(use-feature newsticker
  :preface
  (defun irkalla/newsticker-start-newTab ()
    "Launch NewsTicker (TreeView) in a new tab."
    (interactive)
    (let (success)
      (unwind-protect (progn
                        (tab-bar-new-tab)
                        (call-interactively #'newsticker-treeview)
                        (tab-bar-rename-tab "newsticker")
                        (setq success t))
        (unless success (tab-bar-close-tab)))))

  (defun irkalla/newsticker-quit-newTab ()
    "Quit NewsTicker (TreeView) -> stop NewsTicker -> close tab."
    (interactive)
    (newsticker-treeview-quit)
    (newsticker-stop)
    (tab-close))
  :bind (:map newsticker-treeview-mode-map
              ("o" . newsticker-treeview-browse-url)
              ("q" . irkalla/newsticker-quit-newTab))
  :config
  (when (fboundp 'visual-fill-column-mode)
    (add-hook 'newsticker-treeview-item-mode-hook #'visual-fill-column-mode))
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

  (newsticker-wget-name "curl")
  (newsticker-wget-arguments '("--silent" "--location" "--connect-timeout" "8")))

(provide 'my-readers)
