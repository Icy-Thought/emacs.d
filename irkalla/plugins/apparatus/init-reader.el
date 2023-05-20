;;; toolset/init-reader.el -*- lexical-binding: t -*-

(defgroup irkalla-reader '()
  "A combined Emacs module for our very lovely readers!"
  :tag "Irkalla Reader"
  :group 'irkalla)

;; PDF-Tools: Darker + Width
(use-package pdf-tools
  :elpaca nil
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :hook (pdf-view-mode . pdf-view-themed-minor-mode)
  :custom
  (pdf-loader-install)
  (pdf-view-use-scaling t)
  (pdf-view-use-imagemagick nil)
  (pdf-view-display-size 'fit-width)
  :config
  ;; :HACK: a temporary fix for blinking PDF caused by Evil-Mode!
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (setq-local evil-normal-state-cursor (list nil)))))

(use-package pdf-view-restore
  :hook (pdf-view-mode . pdf-view-restore-mode)
  :custom (pdf-view-restore-filename (expand-file-name "pdf-view-restore" user-emacs-cache-directory)))

;; EPUB Reader!
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :hook ((nov-mode . visual-line-mode)
         (nov-mode . visual-fill-column-mode))
  :custom
  (nov-text-width t)
  (nov-text-width 120))

(use-package nov-xwidget
  :elpaca (:host github :repo "chenyanming/nov-xwidget")
  :after nov
  :bind (:map nov-mode-map
              ("o" . nov-xwidget-view))
  :hook (nov-mode . nov-xwidget-inject-all-files))

;; RSS Feeder for the hungry :P
(use-package elfeed
  :custom
  (elfeed-feeds
   '(("https://sachachua.com/blog/feed/" emacs)
     ("https://www.reddit.com/r/emacs/.rss" emacs)
     ("https://terrytao.wordpress.com/feed/" mathematics)
     ("https://writings.stephenwolfram.com/feed/" mathematics)
     ("https://phys.org/rss-feed/physics-news/" physics)
     ("https://phys.org/rss-feed/breaking/physics-news/" physics))))

(provide 'init-reader)
