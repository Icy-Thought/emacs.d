;;; my-svg-tags.el --- Beautiful Tags For Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-package svg-tag-mode
  :hook ((prog-mode text-mode) . svg-tag-mode)
  :config
  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

  (setopt svg-tag-tags
          ;; :TODO| Reduce to a more general solution
          `((,(rx (group ":" (| "todo" "TODO") "|" (1+ any)))
             . ((lambda (tag) (svg-tag-make tag :face 'ansi-color-yellow :inverse t :crop-left t :beg 6))))

            (,(rx (group ":" (| "todo" "TODO") "|"))
             . ((lambda (tag) (svg-tag-make tag :face 'ansi-color-yellow :inverse nil :margin 0 :crop-right t :beg 1 :end -1))))

            ;; :WARN| Heads-up for whatever insane thing found below.
            (,(rx (group ":" (| "warn" "WARN") "|" (1+ any)))
             . ((lambda (tag) (svg-tag-make tag :face 'warning :inverse t :crop-left t :beg 7))))

            (,(rx (group ":" (| "warn" "WARN") "|"))
             . ((lambda (tag)
                  (svg-tag-make tag :face 'warning :inverse nil :margin 0 :crop-right t :beg 1 :end -1))))

            ;; :FIXME| Fixing this madness cannot wait, get to it!
            (,(rx (group ":" (| "fixme" "FIXME") "|" (1+ any)))
             . ((lambda (tag) (svg-tag-make tag :face 'ansi-color-red :inverse t :crop-left t :beg 7))))

            (,(rx (group ":" (| "fixme" "FIXME") "|"))
             . ((lambda (tag)
                  (svg-tag-make tag :face 'ansi-color-red :inverse nil :margin 0 :crop-right t :beg 1 :end -1))))

            ;; :HACK| Fix this regexp
            ;; :PERF| Fix this regexp
            ;; :MARK| Mark this regexp
            (,(rx (group ":" (| "hack" "HACK" "PERF" "MARK") "|" (1+ any)))
             . ((lambda (tag) (svg-tag-make tag :face 'font-lock-keyword-face :inverse t :crop-left t :beg 6))))

            (,(rx (group ":" (| "hack" "HACK" "PERF" "MARK") "|"))
             . ((lambda (tag)
                  (svg-tag-make tag :face 'font-lock-keyword-face :inverse nil :margin 0 :crop-right t :beg 1 :end -1))))

            ;; :NOTE| Reduce to a more general solution
            (,(rx (group ":" (| "note" "NOTE") "|" (1+ any)))
             . ((lambda (tag) (svg-tag-make tag :face 'ansi-color-green :inverse t :crop-right t :beg 6))))

            (,(rx (group ":" (| "note" "NOTE") "|"))
             . ((lambda (tag) (svg-tag-make tag :face 'ansi-color-green :inverse nil :margin 0 :crop-right t :beg 1 :end -1))))))

  (defun svg-progress-percent (value)
    (svg-image (svg-lib-concat
                (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                      nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                (svg-lib-tag (concat value "%")
                             nil :stroke 0 :margin 0)) :ascent 'center))

  (defun svg-progress-count (value)
    (let* ((seq (mapcar #'string-to-number (split-string value "/")))
           (count (float (car seq)))
           (total (float (cadr seq))))
      (svg-image (svg-lib-concat
                  (svg-lib-progress-bar (/ count total) nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                  (svg-lib-tag value nil :stroke 0 :margin 0)) :ascent 'center)))

  ;; Progress (fraction): [1/3]
  (push `(,(rx (group "[" (1+ digit) "/" (1+ digit) "]"))
          . ((lambda (tag) (svg-progress-count (substring tag 1 -1)))))
        svg-tag-tags)

  ;; Progress (percentage): [45%]
  (push `(,(rx (group "[" (** 1 3 digit) "%]"))
          . ((lambda (tag) (svg-progress-percent (substring tag 1 -2)))))
        svg-tag-tags)

  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local svg-tag-tags
                          ;; Basic tags :THIS:
                          `((,(rx (group ":" (1+ alnum) ":"))
                             . ((lambda (tag) (svg-tag-make tag :face 'org-tag :beg 1 :end -1))))

                            ;; Task priority [#a]
                            (,(rx (group "[#" (1+ word) "]"))
                             . ((lambda (tag) (svg-tag-make tag :face 'org-priority :beg 2 :end -1 :margin 0 :inverse t))))

                            ;; Org TAGS
                            (,(rx (group ":" (| "todo" "TODO") ":"))           . ((lambda (tag) (svg-tag-make "TODO"      :face 'org-todo))))
                            (,(rx (group ":" (| "wip" "WIP") ":"))             . ((lambda (tag) (svg-tag-make "WIP"       :face 'org-cite))))
                            (,(rx (group ":" (| "done" "DONE") ":"))           . ((lambda (tag) (svg-tag-make "DONE"      :face 'org-done))))
                            (,(rx (group ":" (| "note" "NOTE") ":"))           . ((lambda (tag) (svg-tag-make "NOTE"      :face 'org-footnote))))
                            (,(rx (group ":" (| "scheduled" "SCHEDULED") ":")) . ((lambda (tag) (svg-tag-make "SCHEDULED" :face 'org-scheduled))))
                            (,(rx (group ":" (| "deadline" "DEADLINE") ":"))   . ((lambda (tag) (svg-tag-make "DEADLINE"  :face 'org-upcoming-deadline))))

                            ;; Tagging some of Org's many blocks
                            (,(rx (group "#+" (| "name" "NAME") ":"))                   . ((lambda (tag) (svg-tag-make "NAME"            :face 'org-meta-line))))
                            (,(rx (group "#+" (| "begin_src" "BEGIN_SRC")))             . ((lambda (tag) (svg-tag-make "BEGIN SRC"       :face 'org-block-begin-line))))
                            (,(rx (group "#+" (| "end_src" "END_SRC")))                 . ((lambda (tag) (svg-tag-make "END SRC"         :face 'org-block-end-line))))
                            (,(rx (group "#+" (| "begin_export" "BEGIN_EXPORT")))       . ((lambda (tag) (svg-tag-make "BEGIN EXPORT"    :face 'org-block-begin-line))))
                            (,(rx (group "#+" (| "end_export" "END_EXPORT")))           . ((lambda (tag) (svg-tag-make "END EXPORT"      :face 'org-block-end-line))))
                            (,(rx (group "#+" (| "begin_example" "BEGIN_EXAMPLE")))     . ((lambda (tag) (svg-tag-make "BEGIN EXAMPLE"   :face 'org-block-begin-line))))
                            (,(rx (group "#+" (| "end_example" "END_EXAMPLE")))         . ((lambda (tag) (svg-tag-make "END EXAMPLE"     :face 'org-block-end-line))))
                            (,(rx (group "#+" (| "begin_quote" "BEGIN_QUOTE")))         . ((lambda (tag) (svg-tag-make "BEGIN QUOTE"     :face 'org-quote :italic t))))
                            (,(rx (group "#+" (| "end_quote" "END_QUOTE")))             . ((lambda (tag) (svg-tag-make "END QUOTE"       :face 'org-quote :italic t))))
                            (,(rx (group "#+" (| "begin_signature" "BEGIN_SIGNATURE"))) . ((lambda (tag) (svg-tag-make "BEGIN SIGNATURE" :face 'org-footnote :italic t))))
                            (,(rx (group "#+" (| "end_signature" "END_SIGNATURE")))     . ((lambda (tag) (svg-tag-make "END SIGNATURE"   :face 'org-footnote :italic t))))
                            (,(rx (group "#+" (| "begin_sidenote" "BEGIN_SIDENOTE")))   . ((lambda (tag) (svg-tag-make "BEGIN SIDENOTE"  :face 'org-quote :italic t))))
                            (,(rx (group "#+" (| "end_sidenote" "END_SIDENOTE")))       . ((lambda (tag) (svg-tag-make "END SIDENOTE"    :face 'org-quote :italic t))))
                            (,(rx (group "#+" (| "results" "RESULTS") ":"))             . ((lambda (tag) (svg-tag-make "RESULTS"         :face 'org-done :underline nil))))

                            ;; Citation of the form [cite:@Knuth:1984]
                            (,(rx (group "[" (| "cite" "CITE") ":@" (1+ word) ":"))
                             . ((lambda (tag) (svg-tag-make tag :inverse t :beg 7 :end -1 :crop-right t))))

                            (,(rx (seq "[" (| "cite" "CITE") ":@" (1+ word) ":" (group (1+ digit) "]")))
                             . ((lambda (tag) (svg-tag-make tag :end -1 :crop-left t))))

                            ;; :XXX|YYY: -> "XXX" & "YYY"
                            (,(rx (seq (group ":" (1+ upper)) "|" (1+ alnum) ":"))
                             . ((lambda (tag) (svg-tag-make tag :beg 1 :inverse t :margin 0 :crop-right t))))

                            (,(rx (seq ":" (1+ upper) (group "|" (1+ alnum) ":")))
                             . ((lambda (tag) (svg-tag-make tag :beg 1 :end -1 :margin 0 :crop-left t))))

                            ;; Active date <2023-04-03 Sun 17:45>
                            (,(format "\\(<%s>\\)" date-re) .
                             ((lambda (tag) (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))

                            (,(format "\\(<%s \\)%s>" date-re day-time-re) .
                             ((lambda (tag) (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))

                            (,(format "<%s \\(%s>\\)" date-re day-time-re) .
                             ((lambda (tag) (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))

                            ;; Inactive date [2023-04-03 Sun 17:45]
                            (,(format "\\(\\[%s\\]\\)" date-re) .
                             ((lambda (tag) (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))

                            (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
                             ((lambda (tag) (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))

                            (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
                             ((lambda (tag) (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))))
              (defun svg-progress-percent (value)
                (svg-image (svg-lib-concat
                            (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                                  nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                            (svg-lib-tag (concat value "%")
                                         nil :stroke 0 :margin 0)) :ascent 'center))

              (defun svg-progress-count (value)
                (let* ((seq (mapcar #'string-to-number (split-string value "/")))
                       (count (float (car seq)))
                       (total (float (cadr seq))))
                  (svg-image (svg-lib-concat
                              (svg-lib-progress-bar (/ count total) nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                              (svg-lib-tag value nil :stroke 0 :margin 0)) :ascent 'center)))

              ;; Progress (fraction): [1/3]
              (push `(,(rx (group "[" (1+ digit) "/" (1+ digit) "]"))
                      . ((lambda (tag) (svg-progress-count (substring tag 1 -1)))))
                    svg-tag-tags)

              ;; Progress (percentage): [45%]
              (push `(,(rx (group "[" (** 1 3 digit) "%]"))
                      . ((lambda (tag) (svg-progress-percent (substring tag 1 -2)))))
                    svg-tag-tags)
              (svg-tag-mode))))

(provide 'my-svg-tags)
