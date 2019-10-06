(use-package emacs-grammarly
  :straight (emacs-grammarly :type git :host github :repo "mmagnus/emacs-grammarly")
  :bind ("C-c C-g" . grammarly-save-region-and-run))
