(use-package emacs-grammarly
  :straight (emacs-grammarly :type git :host github :repo "mmagnus/emacs-grammarly")
  :bind ("C-c C-g" . grammarly-save-region-and-run))

(use-package github-review
  :straight (github-review :type git :host github :repo "charignon/github-review"))

(use-package devdocs-lookup
  :straight (devdocs-lookup :type git :host github :repo "skeeto/devdocs-lookup")
  :config
  (devdocs-setup))
(use-package package-lint)
