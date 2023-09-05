(use-package elpaca
  :elpaca nil
  :general
  (irkalla/comma-lead-keydef
    "p"   '(:ignore t         :which-key "Package Manager")
    "p p" '(elpaca-manager    :which-key "Open Elpaca")
    "p f" '(elpaca-fetch-all  :which-key "Fetch package commits")
    "p u" '(elpaca-update-all :which-key "Update all packages")))
