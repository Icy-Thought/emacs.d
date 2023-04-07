;;; toolset/init-elfeed.el -*- lexical-binding: t -*-

(defgroup irkalla-elfeed '()
  "an emacs RSS feed for the curious mind."
  :tag "Irkalla Elfeed"
  :group 'irkalla)

(use-package elfeed
  :config
  (setq elfeed-feeds
        '("https://sachachua.com/blog/feed/"
          "https://www.reddit.com/r/emacs/.rss"

          ;; Mathematics
          "https://terrytao.wordpress.com/feed/"
          "https://writings.stephenwolfram.com/feed/"

          ;; Physics
          "https://phys.org/rss-feed/physics-news/"
          "https://phys.org/rss-feed/breaking/physics-news/")))

(provide 'init-elfeed)
