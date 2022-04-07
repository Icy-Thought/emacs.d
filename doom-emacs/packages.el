;; -*- no-byte-compile: t; -*-

;; [[file:config.org::*Screenshot][Screenshot:1]]
(package! screenshot
  :recipe (:host github :repo "tecosaur/screenshot"
           :files ("*.el"))
  :pin "7621e0cd176f65e22bc7e7d03a8003e59426f7f7")
;; Screenshot:1 ends here

;; [[file:config.org::*Prettier Page Breaks][Prettier Page Breaks:1]]
(package! page-break-lines :recipe (:host github :repo "purcell/page-break-lines"))
;; Prettier Page Breaks:1 ends here

(package! org-modern
  :recipe (:host github :repo "minad/org-modern"
           :files ("*.el"))
  :pin "dc19304f409259d1b258c51cedd2d362e0ff9b98")

(package! org-ol-tree
  :recipe (:host github :repo "Townk/org-ol-tree"
           :files ("*.el"))
  :pin "207c748aa5fea8626be619e8c55bdb1c16118c25")

(package! org-transclusion
  :recipe (:host github :repo "nobiot/org-transclusion"
           :files ("*.el"))
  :pin "ccc0aaa72732ea633bf52bcc8a0345cd3ac178fd")

(package! org-pandoc-import
  :recipe (:host github :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))

(package! org-roam :disable t)

(package! org-roam-ui
  :recipe (:host github :repo "org-roam/org-roam-ui"
           :files ("*.el" "out"))
  :pin "309fe3c58c7081de4e2c9c64f7b40ea291926048")

;; Required by Roam-UI
(package! websocket
  :pin "fda4455333309545c0787a79d73c19ddbeb57980")

(package! org-fragtog
  :pin "680606189d5d28039e6f9301b55ec80517a24005")
