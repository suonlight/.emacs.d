;;; -*- lexical-binding: t; -*-

(use-package centaur-tabs
  :defer 1
  :straight (centaur-tabs :type git :host github :repo "ema2159/centaur-tabs")
  :config
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-height 32)
  (setq centaur-tabs-set-icons t)
  ;; (setq centaur-tabs-set-bar 'over)
  (setq centaur-tabs-set-bar 'left)
  (setq centaur-tabs-set-modified-marker t)
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t)
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

 Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
 All buffer name start with * will group to \"Emacs\".
 Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ((memq major-mode '(vterm-mode eshell-mode)) "Terminal")
      ((memq major-mode '(org-mode org-agenda-mode diary-mode)) "OrgMode")
      (t
       (centaur-tabs-get-group-name  (current-buffer))))))
  (defun centaur-tabs-hide-tab (x)
    (let ((name (format "%s" x)))
      (or
       (string-prefix-p "*epc" name)
       (string-prefix-p "*Help" name)
       (string-prefix-p "*helm" name)
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*lsp" name)
       (string-prefix-p "*Ido" name)
       (string-prefix-p "*prodigy" name)
       (string-prefix-p "*RuboCop" name)
       (string-prefix-p "TAGS" name)
       (string-prefix-p "magit" name)
       (string= "*osx-dictonary*" name)
       (string= "*Google Translate*" name)
       (string= "*Flycheck errors*" name)
       (string= "*Flycheck error messages*" name)
       (string= "*Cargo Run*" name)
       (string= "*compilation*" name)
       (string= "*Racer Help*" name)
       (string= "*straight-process*" name)
       (string= "*scratch*" name)
       (string= "*Messages*" name)
       (string= "*Buffer List*" name)
       (string= "*Process List*" name)
       (string= "*sound*" name)
       (string= "*Org-Babel Error Output*" name)
       (string= "*Org Clock*" name)
       (string= "*Org Agenda*" name)
       (string= "*Calendar*" name)
       (string= " *which-key*" name)
       (string= " *LV*" name)
       (and (string-prefix-p "magit" name)
	    (not (file-name-extension name)))
       )))
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode))

(use-package persp-mode
  :after projectile
  :init
  (setq persp-auto-resume-time -1.0 persp-auto-save-opt 0)
  :config
  (defun sl/layout-switch-by-pos (pos)
    "Switch to perspective of position POS."
    (let ((persp-to-switch
	   (nth pos (persp-names-current-frame-fast-ordered))))
      (setq sl/persp-last-layout (safe-persp-name (get-current-persp)))
      (if persp-to-switch
	  (persp-switch persp-to-switch))))

  (dolist (i (number-sequence 9 0 -1))
    (eval `(defun ,(intern (format "persp-switch-to-%s" i)) nil
	     ,(format "Switch to layout %s.\n%s"
		      i "See `layout-switch-by-pos' for details.")
	     (interactive)
	     (sl/layout-switch-by-pos ,(if (eq 0 i) 9 (1- i))))))

  (defvar sl/persp-last-layout "none")
  (defun persp-switch-last-layout ()
    (interactive)
    (let ((persp-current-name (safe-persp-name (get-current-persp))))
      (persp-switch sl/persp-last-layout)
      (setq sl/persp-last-layout persp-current-name)))

  (defun sl/persp-layout ()
    "Switch to perspective of position POS."
    (interactive)
    (let* ((persp-current-name (safe-persp-name (get-current-persp)))
	   (highlight-persps (lambda (elt idx)
			       (if (string= elt persp-current-name )
				   (propertize
				    (format "%d:%s" (+ idx 1) (f-base elt))
				    'face '(:foreground "red" :background "yellow"))
				 (format "%d:%s" (+ idx 1) (f-base elt))))))
      (string-join (seq-map-indexed highlight-persps (persp-names-current-frame-fast-ordered)) " | ")))

  (defhydra sl/persp-hydra
    (:color pink :hint nil :exit t)
    "Layout: %s(sl/persp-layout)"
    ("n" persp-next "Next Layout" :column "Go to")
    ("p" persp-prev "Prev Layout")
    ("l" persp-switch "Switch Layout")
    ("0" persp-switch-to-0)
    ("1" persp-switch-to-1)
    ("2" persp-switch-to-2)
    ("3" persp-switch-to-3)
    ("4" persp-switch-to-4)
    ("5" persp-switch-to-5)
    ("6" persp-switch-to-6)
    ("7" persp-switch-to-7)
    ("8" persp-switch-to-8)
    ("9" persp-switch-to-9)
    ("<tab>" persp-switch-last-layout)

    ("d" persp-kill :column "Actions")
    ("r" persp-rename)
    ("s" persp-save-state-to-file "Save Layout")
    ("L" persp-load-state-from-file "Load Layout")
    ("q" nil "cancel" :color blue :column nil))
  (persp-mode 1))

(defun ivy-persp-switch-project (arg)
  (interactive "P")
  (ivy-read "Switch to Project Perspective: "
	    (if (projectile-project-p)
		(cons (abbreviate-file-name (projectile-project-root))
		      (projectile-relevant-known-projects))
	      projectile-known-projects)
	    :action (lambda (project)
		      (let* ((persp-reset-windows-on-nil-window-conf t)
			     (exists (persp-with-name-exists-p project)))
			(persp-switch project)
			(unless exists
			  (progn
			    (let ((projectile-completion-system 'ivy))
			      (projectile-switch-project-by-name project))))))))
