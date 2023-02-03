;;; init.el -*- lexical-binding: t; -*-

;; Profile Emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Irkalla loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                               (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; GC pauses -> faster (decrease threshold)
(setq gc-cons-threshold (* 2 1000 1000))

;; Initialize --debug-init on error
(setq debug-on-error init-file-debug)

;;; Load `lisp` & `plugins` direcotries into path
(let ((lisp-dir (expand-file-name "lisp" user-emacs-directory)) 
      (plugins-dir (expand-file-name "plugins" user-emacs-directory)))
  (dolist (dir (list lisp-dir plugins-dir))
    (add-to-list 'load-path dir)))

;;; (bootstrap) straight.el
(require 'init-straight)

;; data-related files belongs in `$XDG_DATA_HOME`...
(setq user-emacs-directory "~/.local/share/emacs")

;; Customization -> /tmp/emacs-custom-*.el
(setq custom-file
      (if (boundp 'server-socket-dir)
        (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;;; (no-littering) A cleaner Emacs directory
(use-package no-littering
    :init
    (setq no-littering-etc-directory "~/.local/share/emacs/etc"
          no-littering-var-directory "~/.local/share/emacs/var"))

;; Testing: lisp modules
(require 'options)
(require 'shortcuts)

;; Testing: plugins modules
(require 'init-evil)
