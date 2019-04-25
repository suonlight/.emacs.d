;;; -*- lexical-binding: t; -*-
;; (setq inhibit-startup-screen t)
;; (setq initial-scratch-message ";; Happy Hacking")
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

(setq-default truncate-lines nil)
(setq-default global-visual-line-mode t)
(setq make-backup-files nil)
;; (toggle-frame-fullscreen)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)
(setq tags-add-tables nil)

(setq mode-require-final-newline t)
(setq require-final-newline t)

;; (setq mac-option-key-is-meta nil
;;       mac-command-key-is-meta t
;;       mac-command-modifier 'meta
;;       mac-option-modifier 'none)

;; (setq mac-command-modifier 'meta
;;       mac-option-modifier 'none)

(setq use-package-verbose t)
