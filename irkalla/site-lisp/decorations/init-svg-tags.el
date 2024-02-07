;;; init-svg-tags.el --- SVG-Tags: Button-related Tagging -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://icy-thought.github.io/

;;; Commentary:
;; SVG is very powerful (thanks, math! xD) and thanks to it's tech we are able to create good-looking buttons for our
;; Emacs experience.

;;; Code:

(use-package svg-tag-mode
  :requires (svg-lib)
  :hook ((prog-mode text-mode) . svg-tag-mode)
  :preface
  (defun svg-progress-percent (value)
    (svg-image (svg-lib-concat
                (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                      nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                (svg-lib-tag (concat value "%")
                             nil :stroke 0 :margin 0)) :ascent 'center)
    
    (defun svg-progress-count (value)
      (let* ((seq (mapcar #'string-to-number (split-string value "/")))
             (count (float (car seq)))
             (total (float (cadr seq))))
        (svg-image (svg-lib-concat
                    (svg-lib-progress-bar (/ count total) nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                    (svg-lib-tag value nil :stroke 0 :margin 0)) :ascent 'center))))

  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))
  :custom
  (svg-tag-tags
   `(
     ;; Org tags :THIS:
     ;; ("\\(:[A-Za-z0-9]+:\\)" . ((lambda (tag)
     ;;                              (svg-tag-make tag :beg 1 :end -1 :inverse t))))
     
     ;; Task priority [#a]
     ("\\[#[a-zA-Z]\\]" . ((lambda (tag)
                             (svg-tag-make tag :face 'org-priority :beg 2 :end -1 :margin 0 :inverse t))))
     
     ;; Progress (percentage): [45%]
     ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                         (svg-progress-percent (substring tag 1 -2)))))
     
     ;; Progress (fraction): [1/3]
     ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                       (svg-progress-count (substring tag 1 -1)))))
     
     ;; Specific tags -> allow spaces
     ;; :TODO| Reduce to a more general solution
     ;; :NOTE| Reduce to a more general solution
     ;; :FIXME| There is more regexp
     ;; :HACK| Fix this regexp
     ;; :WARN| This needs to be fixed
     
     ("\\([:]\\(?:TODO\\|todo\\)|.*\\)" . ((lambda (tag)
                                             (svg-tag-make tag :face 'org-todo :inverse t :crop-left t :beg 6))))
     
     ("\\([:]\\(?:TODO\\|todo\\)*|\\)" . ((lambda (tag)
                                            (svg-tag-make tag :face 'org-todo :inverse nil :margin 0 :crop-right t :beg 1 :end -1))))
     
     ("\\([:]\\(?:WARN\\|warn\\)|.*\\)" . ((lambda (tag)
                                             (svg-tag-make tag :face 'org-priority :inverse t :crop-left t :beg 7))))
     
     ("\\([:]\\(?:FIXME\\|fixme\\)|.*\\)" . ((lambda (tag)
                                               (svg-tag-make tag :face 'org-upcoming-deadline :inverse t :crop-left t :beg 7))))

     ("\\([:]\\(?:FIXME\\|fixme\\)*|\\)" . ((lambda (tag)
                                              (svg-tag-make tag :face 'org-upcoming-deadline :inverse nil :margin 0 :crop-right t :beg 1 :end -1))))
     
     ("\\([:]\\(?:HACK\\|PERF\\|MARK\\|hack\\)|.*\\)" . ((lambda (tag)
                                                           (svg-tag-make tag :face 'org-priority :inverse t :crop-left t :beg 6))))
     
     ("\\([:]\\(?:HACK\\|hack\\|PERF\\|WARN\\|warn\\|MARK\\)*|\\)" . ((lambda (tag)
                                                                        (svg-tag-make tag :face 'org-priority :inverse nil :margin 0 :crop-right t :beg 1 :end -1))))
     
     ("\\([:]\\(?:NOTE\\|note\\)|.*\\)" . ((lambda (tag)
                                             (svg-tag-make tag :face 'org-footnote :inverse t :crop-right t :beg 6))))
     
     ("\\([:]\\(?:NOTE\\|note\\)*|\\)" . ((lambda (tag)
                                            (svg-tag-make tag :face 'org-footnote :inverse nil :margin 0 :crop-right t :beg 1 :end -1))))
     
     ;; Org TAGS
     (":TODO:"              . ((lambda (tag) (svg-tag-make "TODO"         :face 'org-todo))))
     (":WIP:"               . ((lambda (tag) (svg-tag-make "WIP"          :face 'org-cite))))
     (":DONE:"              . ((lambda (tag) (svg-tag-make "DONE"         :face 'org-done))))
     (":NOTE:"              . ((lambda (tag) (svg-tag-make "NOTE"))))
     ("SCHEDULED:"          . ((lambda (tag) (svg-tag-make "SCHEDULED"    :face 'org-warning))))
     ("DEADLINE:"           . ((lambda (tag) (svg-tag-make "DEADLINE"     :face 'org-priority))))
     ("^#\\+begin_src"      . ((lambda (tag) (svg-tag-make "BEGIN_SRC"    :face 'org-block-begin-line))))
     ("^#\\+end_src"        . ((lambda (tag) (svg-tag-make "END_SRC"      :face 'org-block-end-line))))
     ("^#\\+begin_export"   . ((lambda (tag) (svg-tag-make "BEGIN_EXPORT" :face 'org-block-begin-line))))
     ("^#\\+end_export"     . ((lambda (tag) (svg-tag-make "END_EXPORT"   :face 'org-block-end-line))))
     ("^#\\+begin_example"  . ((lambda (tag) (svg-tag-make "EXAMPLE"      :face 'org-block-begin-line))))
     ("^#\\+end_example"    . ((lambda (tag) (svg-tag-make "EXAMPLE"      :face 'org-block-end-line))))
     ("^#\\+begin_quote"    . ((lambda (tag) (svg-tag-make "QUOTE"        :face 'org-quote :italic t))))
     ("^#\\+end_quote"      . ((lambda (tag) (svg-tag-make "QUOTE"        :face 'org-quote :italic t))))
     ("^#\\+begin_sidenote" . ((lambda (tag) (svg-tag-make "SIDENOTE"     :face 'org-quote :italic t))))
     ("^#\\+end_sidenote"   . ((lambda (tag) (svg-tag-make "SIDENOTE"     :face 'org-quote :italic t))))
     ("^#\\+RESULTS:"       . ((lambda (tag) (svg-tag-make "RESULTS"      :face 'org-done :underline nil))))
     (":X"                  . ((lambda (tag) (svg-tag-make "[X]"          :face 'org-checkbox-statistics-done))))
     (":-"                  . ((lambda (tag) (svg-tag-make "[-]"          :face 'org-checkbox))))
     
     ;; Citation of the form [cite:@Knuth:1984]
     ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                       (svg-tag-make tag :inverse t :beg 7 :end -1 :crop-right t))))
     
     ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                (svg-tag-make tag :end -1 :crop-left t))))
     
       ;;; Works for stuff like :XXX|YYY:
     ("\\(:[A-Z]+\\)\|[a-zA-Z#0-9]+:" . ((lambda (tag)
                                           (svg-tag-make tag :beg 1 :inverse t :margin 0 :crop-right t))))
     
     (":[A-Z]+\\(\|[a-zA-Z#0-9]+:\\)" . ((lambda (tag)
                                           (svg-tag-make tag :beg 1 :end -1 :margin 0 :crop-left t))))
     
     ;; Active date (with or without day name, with or without time) <2023-04-03 Sun 17:45>
     (,(format "\\(<%s>\\)" date-re) . ((lambda (tag)
                                          (svg-tag-make tag :beg 1 :end -1 :margin 0))))
     
     (,(format "\\(<%s \\)%s>" date-re day-time-re) . ((lambda (tag)
                                                         (svg-tag-make tag :beg 1 :inverse t :crop-right t :margin 0 :face 'org-agenda-date))))
     
     (,(format "<%s \\(%s>\\)" date-re day-time-re) . ((lambda (tag)
                                                         (svg-tag-make tag :end -1 :inverse nil :crop-left t :margin 0 :face 'org-agenda-date)))))))

(provide 'init-svg-tags)
;;; init-svg-tags.el ends here
