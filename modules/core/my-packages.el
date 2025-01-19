;;; my-packages.el --- Emacs's Built-in Package Manager -*- lexical-binding: t -*-

;; Copyright (C) 2023-2025 Icy-Thought

;; Author: Icy-Thought <icy-thought@pm.me>
;; Keywords: internal
;; URL: https://github.com/Icy-Thought/emacs.d/

(require 'package)

(setopt package-archives
        '(("melpa" . "https://melpa.org/packages/")
          ("org" . "https://orgmode.org/elpa/")
          ("elpa" . "https://elpa.gnu.org/packages/")
          ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; :NOTE| Configuring use-package to function as intended

(setopt use-package-vc-prefer-newest t
        use-package-always-ensure t
        use-package-verbose t
        use-package-compute-statistics t)

(provide 'my-packages)
