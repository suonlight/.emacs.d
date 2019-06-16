(use-package ruby-test-mode
  :hook (ruby-mode . ruby-test-mode)
  :after ruby-mode)

(use-package ruby-end
  :hook (ruby-mode . ruby-end-mode)
  :after ruby-mode
  :custom
  (ruby-end-insert-newline nil))

(use-package inf-ruby
  :after ruby-mode
  :hook (ruby-mode . inf-ruby-minor-mode))

;; (autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
;; (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

(use-package rubocop
  :after ruby-mode
  :hook (ruby-mode . rubocop-mode))

(use-package rbenv
  :after ruby-mode
  :hook (ruby-mode . global-rbenv-mode))
