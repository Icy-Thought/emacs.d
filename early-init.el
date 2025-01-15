;;; early-init.el --- Welcome To Irkalla Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(defgroup ikralla nil
  "Irkalla might as well become a cult at this point."
  :link '(url-link "https://github.com/Icy-Thought/emacs.d")
  :group 'emacs)

(defcustom irkalla/underworld (file-truename "~/Workspace/public/emacs.d")
  "Underworld, the land where Irkalla resides within."
  :type 'string
  :group 'irkalla)

(setq-default user-emacs-directory "~/.config/emacs")

;; :NOTE| adding our directories to the Emacs load-path

(let ((default-directory irkalla/underworld))
  (when (file-exists-p default-directory)
    (normal-top-level-add-to-load-path '("modules"))
    (normal-top-level-add-subdirs-to-load-path)))

(setq-default custom-file
              (expand-file-name "etc/custom.el" user-emacs-directory))

(if (file-exists-p custom-file)
    (load custom-file 'noerror 'nomessage)
  (with-temp-buffer (write-file custom-file)))

;; :NOTE| Perserved buffers per session

(with-current-buffer "*scratch*"  (emacs-lock-mode 'kill))
(with-current-buffer "*Messages*" (emacs-lock-mode 'kill))

;; :NOTE| Increase CPU processing restrictions

(when (boundp 'read-process-output-max)
  (setq-default process-adaptive-read-buffering nil
                read-process-output-max
                (or (ignore-errors (with-temp-buffer
                                     (insert-file-contents "/proc/sys/fs/pipe-max-size")
                                     (string-to-number (buffer-string))))
                    (* 1024 1024))))

;; :NOTE| Compile Emacs natively for better performance

(when (featurep 'native-compile)
  (let ((path (expand-file-name "var/eln-cache/" user-emacs-directory)))
    (setq-default native-comp-eln-load-path (list path)
                  native-compile-target-directory path))

  (setq-default native-comp-deferred-compilation nil
                native-comp-async-report-warnings-errors nil))

(setq-default gc-cons-percentage 0.6
              gc-cons-threshold most-positive-fixnum)

;; :NOTE| Reduce GC threshold for propper garbage collection

(add-hook 'after-init-hook
          (lambda () (setopt gc-cons-threshold (* 5 1024 1024))))

;; :NOTE| Push file handlers to a later stage -> faster upstart

(put 'file-name-handler-alist 'original-value
     (default-toplevel-value 'file-name-handler-alist))
(set-default-toplevel-value 'file-name-handler-alist nil)

;; Restore `file-name-handler-alist' after startup while conserving the potential new elements
(add-hook 'emacs-startup-hook
          (lambda ()
            (setopt file-name-handler-alist
                    (delete-dups
                     (append file-name-handler-alist
                             (get 'file-name-handler-alist 'original-value))))) 99)

;; :NOTE| Debugging & error handling

(setq-default ad-redefinition-action 'accept
              debug-on-error init-file-debug
              jka-compr-verbose init-file-debug)

;; :NOTE| User interface & Emacs startup

(setq-default auto-mode-case-fold nil
              blink-cursor-mode nil
              echo-keystrokes 0.02
              fast-but-imprecise-scrolling t
              inhibit-startup-screen t
              menu-bar-mode nil
              mode-line-format nil
              scroll-bar-mode nil
              tool-bar-mode nil
              use-dialog-box nil
              use-file-dialog nil)

;; :NOTE| Reduce startup blabbering aggressively

(fset 'display-startup-echo-area-message #'ignore)
(fset 'display-startup-screen #'ignore)

;; :NOTE| Tweaking the aesthetics a little

(setq-default default-frame-alist
              (append '((alpha-background     . 85)
                        (fullscreen           . nil)
                        (menu-bar-lines       . 0)
                        (tool-bar-lines       . 0)
                        (vertical-scroll-bars . nil)))
              initial-frame-alist (copy-alist default-frame-alist)
              frame-inhibit-implied-resize t
              frame-resize-pixelwise t
              idle-update-delay 1.0
              inhibit-compacting-font-caches t
              redisplay-skip-fontification-on-input t)

;; :NOTE| File handeling & version control

(setq-default auto-save-list-file-prefix nil
              create-lockfiles nil
              package-enable-at-startup nil
              use-short-answers t
              vc-follow-symlinks t)

;; :NOTE| Miscellaneous changes

(setq-default initial-major-mode 'fundamental-mode
              initial-scratch-message nil
              load-prefer-newer t
              ring-bell-function 'ignore
              select-active-regions 'only
              select-enable-clipboard nil)

;; :NOTE| Set default Emacs encoding

(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(set-default-coding-systems 'utf-8)
(set-language-environment "UTF-8")
(set-locale-environment "en_US.UTF-8")

(provide 'early-init)
