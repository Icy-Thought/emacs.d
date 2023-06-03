;;; lisp/init-nixpkgs.el -*- lexical-binding: t -*-

;; Autoload Nix installed packages (built-in) properly!
(dolist (path load-path)
  (when (string-match-p "/nix/store/[a-z0-9]\\{32\\}-emacs-packages-deps.*" path)
    (dolist (autoload-file (directory-files path t "-autoloads.el"))
      (with-demoted-errors "init.el error: %s"
        (load autoload-file nil t)))))

(provide 'init-nixpkgs)
