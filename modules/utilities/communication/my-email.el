;;; my-email.el --- E-mail Client For Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(use-feature mu4e
  :if (executable-find "mu")
  :commands (mu4e mu4e-update-mail-and-index)
  :init (add-to-list 'elpaca-ignored-dependencies 'mu4e)
  :custom
  (mu4e-attachment-dir "~/Downloads")
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-update-interval (* 10 60))
  (mu4e-confirm-quit nil)
  (mu4e-use-fancy-chars t)
  (mu4e-notification-support t)
  (mu4e-change-filenames-when-moving t)
  (mu4e-completing-read-function 'completing-read)
  (mu4e-context-policy 'pick-first)
  (mu4e-headers-date-format "%d-%m")
  (mu4e-headers-time-format "%H:%M")

  ;; :NOTE| control how e-mails should be composed
  (sendmail-program "msmtp")
  (send-mail-function #'sendmail-send-it)
  (message-fill-column fill-column)
  (message-kill-buffer-on-exit t))

;; :NOTE| Reduce the burden of managing multiple accounts

(use-package mu4easy
  :after (mu4e)
  :config (mu4easy-mode)
  :custom
  (mu4easy-contexts '((mu4easy-context
                       :c-name  "Disroot"
                       :maildir "icy-thought"
                       :mail    "IcyThought@disroot.org"
                       :smtp    "disroot.org"
                       :sent-action delete))))

;; :NOTE| Threaded conversations

(use-package mu4e-thread-folding
  :ensure (:host github :repo "rougier/mu4e-thread-folding")
  :after (mu4e)
  :hook (mu4e-headers-mode . mu4e-thread-folding-mode)
  :bind (:map mu4e-headers-mode-map
              ("<tab>"     . mu4e-headers-toggle-at-point)
              ("<left>"    . mu4e-headers-fold-at-point)
              ("<S-left>"  . mu4e-headers-fold-all)
              ("<right>"   . mu4e-headers-unfold-at-point)
              ("<S-right>" . mu4e-headers-unfold-all))
  :config
  (add-to-list 'mu4e-header-info-custom
               '(:empty . (:name "Empty"
                           :shortname ""
                           :function (lambda (msg) "  "))))
  :custom
  (mu4e-headers-fields '((:empty        .  2)
                         (:human-date   . 12)
                         (:flags        .  6)
                         (:mailing-list . 10)
                         (:from         . 22)
                         (:subject      . nil))))

;; :NOTE| Compose E-mails through Org-Mode

(use-package org-msg
  :after (mu4e org)
  :config
  (setopt mail-user-agent 'mu4e-user-agent)
  (org-msg-mode-mu4e)
  (org-msg-mode)
  :custom
  (org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil title:nil email:nil tex:imagemagick")
  (org-msg-startup "hidestars indent inlineimages")
  (org-msg-greeting-fmt "\nGreetings %s,\n\n")
  (org-msg-greeting-name-limit 3)
  (org-msg-default-alternatives
   '((new           . (utf-8 html))
     (reply-to-text . (utf-8))
     (reply-to-html . (utf-8 html))))
  (org-msg-convert-citation t)
  (org-msg-signature "
Kind Regards,

,#+begin_signature
-- *Icy-Thought*
,#+end_signature"))

(provide 'my-email)
