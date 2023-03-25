org-mode

(caption "#+caption: ")
(drawer ":" p ":" n r ":end:")
(begin "#+begin_" (s name) n> r> n "#+end_" name)
(quote "#+begin_quote" n> r> n "#+end_quote")
(sidenote "#+begin_sidenote" n> r> n "#+end_sidenote")
(marginnote "#+begin_marginnote" n> r> n "#+end_marginnote")
(example "#+begin_example" n> r> n "#+end_example")
(center "#+begin_center" n> r> n "#+end_center")
(ascii "#+begin_export ascii" n> r> n "#+end_export")
(html "#+begin_export html" n> r> n "#+end_export")
(latex "#+begin_export latex" n> r> n "#+end_export")
(comment "#+begin_comment" n> r> n "#+end_comment")
(verse "#+begin_verse" n> r> n "#+end_verse")
(src "#+begin_src " q " :async" n> r> n "#+end_src")
(gnuplot "#+begin_src gnuplot :var data=" (p "table") " :file " (p "plot.png") n> r> n "#+end_src" :post (org-edit-src-code))
(elisp "#+begin_src emacs-lisp" n> r> n "#+end_src" :post (org-edit-src-code))
(inlsrc "src_" p "{" q "}")
(title "#+title: " p n "#+author: Icy-Thought" n "#+language: en")

;; Local Variables:
;; mode: lisp-data
;; outline-regexp: "[a-z]"
;; End:
