;;; -*- lexical-binding: t; -*-

;; Package configs
(require 'package)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")))

(defvar bootstrap-version)
(let ((bootstrap-file

       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package benchmark-init :config (add-hook 'after-init-hook #'benchmark-init/deactivate))

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
	  (setq gc-cons-threshold 16777216
		gc-cons-percentage 0.1))

(require 'cl)
(load (concat user-emacs-directory "core/base"))
(load (concat user-emacs-directory "core/core"))
(org-babel-load-file (concat user-emacs-directory "core/emacs.org"))
(load (concat user-emacs-directory "core/workspace"))
(load (concat user-emacs-directory "core/terminal"))

(let ((private-file (concat user-emacs-directory "private/init.el")))
  (when (file-exists-p private-file)
    (message "Load private files...")
    (load private-file)))
