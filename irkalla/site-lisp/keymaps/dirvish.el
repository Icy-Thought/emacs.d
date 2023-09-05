(irkalla/comma-lead-keydef
  "f"   '(:ignore t    :which-key "Dirvish")
  "f f" '(dirvish-side :which-key "Dirvish Side-View")
  "f /" '(dirvish-fd   :which-key "Run fd in dir"))

(irkalla/comma-lead-keydef
  :keymaps 'dirvish-mode-map
  "a"   '(dirvish-quick-access        :which-key "Frequently used dirs")
  "f"   '(dirvish-file-info-menu      :which-key "File information")
  "y"   '(dirvish-yank-menu           :which-key "Yank marked files")
  "N"   '(dirvish-narrow              :which-key "Live narrowing")
  "^"   '(dirvish-history-last        :which-key "Goto recent buffer")
  "h"   '(dirvish-history-jump        :which-key "Goto recent dirs") ; remapped `describe-mode'
  "s"   '(dirvish-quicksort           :which-key "Sort buffers")     ; remapped `dired-sort-toggle-or-edit'
  "v"   '(dirvish-vc-menu             :which-key "Version control")  ; remapped `dired-view-file'
  "TAB" '(dirvish-subtree-toggle      :which-key "Dir -> sub-tree")
  "M-f" '(dirvish-history-go-forward  :which-key "History -> forward")
  "M-b" '(dirvish-history-go-backward :which-key "History -> backward")
  "M-l" '(dirvish-ls-switches-menu    :which-key "Setup listing switches")
  "M-m" '(dirvish-mark-menu           :which-key "Manage Marks")
  "M-t" '(dirvish-layout-toggle       :which-key "Toggle Fullscreen")
  "M-s" '(dirvish-setup-menu          :which-key "User Interface Setup")
  "M-e" '(dirvish-emerge-menu         :which-key "Manage Emerged Groups")
  "M-j" '(dirvish-fd-jump             :which-key "Setup fd-find Switches"))
