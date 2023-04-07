;;; init.el -*- lexical-binding: t; -*-

;; GC pauses -> faster (decrease threshold)
(setq gc-cons-threshold (* 2 1000 1000))

;; Initialize --debug-init on error
(setq debug-on-error init-file-debug)

;; Defining our emacs folders:
(require 'xdg)

(setq-default
 user-emacs-config-directory (expand-file-name "emacs" (xdg-config-home))
 user-emacs-data-directory (expand-file-name "emacs" (xdg-data-home))
 user-emacs-cache-directory (expand-file-name "emacs" (xdg-cache-home)))

;; Specifying our cache & backup dir
(let ((backup-dir (expand-file-name "backup" user-emacs-cache-directory))
      (auto-save-dir (expand-file-name "auto-save" user-emacs-cache-directory)))
  (unless (file-directory-p backup-dir)
    (mkdir backup-dir t)
    (mkdir auto-save-dir t))

  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-save-dir t))
        create-lockfiles nil
        backup-by-copying t))

;; Customization -> /tmp/emacs-custom-*.el
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))

(when (file-exists-p custom-file)
  (load custom-file))

;;; Load our Irkalla config directory
(defconst irkalla-directory "~/git/icy-thought/emacs.d/irkalla")

;; Add =irkalla-directory= + dirs to path!
(add-to-list 'load-path irkalla-directory)
(add-to-list 'load-path (expand-file-name "lisp" irkalla-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" irkalla-directory))

;; Essentials: structured Emacs modules
(require 'init-straight)
(require 'init-management)
(require 'init-decorations)
(require 'init-options)
(require 'init-shortcuts)

;; Add all directories inside "plugins/" to path
(let ((plugins-dir (expand-file-name "plugins" irkalla-directory)))
  (dolist (dir (directory-files plugins-dir t "^[^.].*" 'nosort))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))

;; Additional: category-based modules
(require 'init-apparatus)
(require 'init-completion)
(require 'init-editor)
(require 'init-langserv)
(require 'init-ui)

(provide 'init)
