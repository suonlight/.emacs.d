;;; -*- lexical-binding: t; -*-

(use-package which-key
  :defer .1
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+")
  :config (which-key-mode))

(use-package evil
  ;; :defer .1
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-search-module 'evil-search)
  :config
  (evil-define-text-object evil-inner-buffer (count &optional beg end type)
    (list (point-min) (point-max)))

  (define-key evil-inner-text-objects-map "g" #'evil-inner-buffer)
  (define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)

  (defalias #'forward-evil-word #'forward-evil-symbol)
  (evil-mode 1))

(use-package evil-commentary
  :after evil
  :bind (:map evil-normal-state-map
	      ("gc" . evil-commentary)))

(use-package evil-surround
  :after evil
  ;; :commands (evil-surround-edit evil-Surround-edit evil-surround-region evil-Surround-region)
  :init
  (evil-define-key 'visual global-map "S" 'evil-surround-region)
  (evil-define-key 'visual global-map "gS" 'evil-Surround-region)
  :config (global-evil-surround-mode))

(use-package evil-indent-plus
  :after evil
  :config (evil-indent-plus-default-bindings))

(use-package evil-matchit
  :after evil
  :config (global-evil-matchit-mode))

(use-package evil-goggles
  :after evil
  :config (evil-goggles-mode))

(use-package evil-string-inflection :after evil :commands evil-operator-string-inflection)

(use-package avy
  :commands (avy-pop-mark avy-resume evil-avy-goto-char-timer evil-avy-goto-word-or-subword-1 evil-avy-goto-char-2)
  :config (avy-setup-default))

(use-package dumb-jump
  :commands (dumb-jump-go-other-window dumb-jump-go dumb-jump-back dumb-jump-go-prefer-external dumb-jump-go-prefer-external-other-window)
  :init
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-prefer-searcher 'rg))

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :custom
  (flycheck-highlighting-mode 'symbols)
  (flycheck-indication-mode nil))

(setq-default flycheck-disabled-checkers '(ruby-reek emacs-lisp emacs-lisp-checkdoc javascript-jshint))

(use-package indent-guide
  :hook ((ruby-mode . indent-guide-mode)
	 (yaml-mode . indent-guide-mode)
	 ;; (js2-mode . indent-guide-mode)
	 (js-mode . indent-guide-mode)))


(use-package iedit :bind ("C-;" . iedit-mode))

(use-package evil-iedit-state
  :after iedit
  :bind ("C-;" . evil-iedit-state/iedit-mode))


(use-package yasnippet :hook (after-init . yas-global-mode))

(use-package smartparens
  :hook (after-init . smartparens-global-mode))

(use-package editorconfig
  :delight editorconfig-mode
  :hook (after-init . editorconfig-mode))


(use-package move-text
  ;; :config (move-text-default-bindings)
  :bind (
	 ;; :map evil-visual-state-map ("s-p" . move-text-region-up) ("s-n" . move-text-region-down)
	 :map evil-normal-state-map
	 ("s-p" . move-text-line-up)
	 ("s-n" . move-text-line-down)))

(load (concat user-emacs-directory "core/binding"))
(load (concat user-emacs-directory "core/core-lib"))
(load (concat user-emacs-directory "core/core-projects"))
(load (concat user-emacs-directory "core/core-git"))
(load (concat user-emacs-directory "core/core-ui"))
(load (concat user-emacs-directory "core/org-mode"))
