;;; -*- lexical-binding: t; -*-

(use-package awesome-tab
  :defer 1
  :straight (awesome-tab :type git :host github :repo "manateelazycat/awesome-tab")
  :init
  (setq awesome-tab-style 'zigzag)
  :config
  (defun awesome-tab-buffer-groups ()
    "`awesome-tab-buffer-groups' control buffers' group rules.

	       Group awesome-tab with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
	       All buffer name start with * will group to \"Emacs\".
	       Other buffer group by `awesome-tab-get-group-name' with project name."
    (list
     (cond
      ((memq major-mode '(vterm-mode eshell-mode)) "Terminal")
      ((memq major-mode '(org-mode org-agenda-mode diary-mode)) "OrgMode")
      (t
       (awesome-tab-get-group-name (current-buffer))))))
  (defun awesome-tab-hide-tab (x)
    (let ((name (format "%s" x)))
      (or
       (string-prefix-p "*epc" name)
       (string-prefix-p "*Help" name)
       (string-prefix-p "*helm" name)
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*lsp" name)
       (string-prefix-p "*Ido" name)
       (string-prefix-p "*prodigy" name)
       (string-prefix-p "TAGS" name)
       (string= "*Google Translate*" name)
       (string= "*Flycheck error messages*" name)
       (string= "*Cargo Run*" name)
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
  (defun awesome-tab-click-to-tab (event)
    "Switch to buffer (obtained from EVENT) on clicking header line"
    (interactive "e")
    (let ((position (event-start event)))
      (select-window (posn-window position))
      (let ((selected-tab-name
	     (string-trim (car (posn-string position)))))
	(unless (string-match-p "^%-$" selected-tab-name)
	  (switch-to-buffer selected-tab-name)))))
  (global-set-key [header-line mouse-1] #'awesome-tab-click-to-tab)

  (defun sl/awesome-tab-switch-group-by-pos (pos)
    "Switch to perspective of position POS."
    (let ((group-to-switch
	   (nth pos (awesome-tab-get-groups))))
      (if group-to-switch
	  (awesome-tab-switch-group group-to-switch))))

  (dolist (i (number-sequence 9 0 -1))
    (eval `(defun ,(intern (format "awesome-tab-switch-group-to-%s" i)) nil
	     ,(format "Switch to layout %s.\n%s"
		      i "See `awesome-tab-switch-group-by-pos' for details.")
	     (interactive)
	     (sl/awesome-tab-switch-group-by-pos ,(if (eq 0 i) 9 (1- i))))))

  (defun sl/awesome-group-tab-layout ()
    "Switch to perspective of position POS."
    (interactive)
    (let* ((group-current-name (awesome-tab-current-tabset t))
	   (highlight-groups (lambda (elt idx)
			       (if (string= elt group-current-name)
				   (propertize
				    (format "%d:%s" (+ idx 1) (f-base elt))
				    'face '(:foreground "red" :background "yellow"))
				 (format "%d:%s" (+ idx 1) (f-base elt))))))
      (string-join (seq-map-indexed highlight-groups (awesome-tab-get-groups)) " | ")))

  (defhydra sl/awesome-tab-group-hydra
    (:color pink :hint nil :exit t)
    "Layout: %s(sl/awesome-group-tab-layout)"
    ("n" awesome-tab-forward-group "Next Layout" :column "Go to")
    ("p" awesome-tab-backward-group "Prev Layout")
    ("l" awesome-tab-switch-group "Switch Layout")
    ("0" awesome-tab-switch-group-to-0)
    ("1" awesome-tab-switch-group-to-1)
    ("2" awesome-tab-switch-group-to-2)
    ("3" awesome-tab-switch-group-to-3)
    ("4" awesome-tab-switch-group-to-4)
    ("5" awesome-tab-switch-group-to-5)
    ("6" awesome-tab-switch-group-to-6)
    ("7" awesome-tab-switch-group-to-7)
    ("8" awesome-tab-switch-group-to-8)
    ("9" awesome-tab-switch-group-to-9)
    ("d" awesome-tab-kill-all-buffers-in-current-group "Delete buffers in Group" :column "Actions")
    ("q" nil "cancel" :color blue :column nil))

  (awesome-tab-mode t))

(use-package persp-mode
  :after projectile
  :init
  (setq persp-auto-resume-time -1.0 persp-auto-save-opt 0)
  :config
  (defun sl/layout-switch-by-pos (pos)
    "Switch to perspective of position POS."
    (let ((persp-to-switch
	   (nth pos (persp-names-current-frame-fast-ordered))))
      (if persp-to-switch
	  (persp-switch persp-to-switch))))

  (dolist (i (number-sequence 9 0 -1))
    (eval `(defun ,(intern (format "persp-switch-to-%s" i)) nil
	     ,(format "Switch to layout %s.\n%s"
		      i "See `layout-switch-by-pos' for details.")
	     (interactive)
	     (sl/layout-switch-by-pos ,(if (eq 0 i) 9 (1- i))))))

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

    ("d" persp-kill :column "Actions")
    ("r" persp-rename)
    ("s" persp-save-state-to-file "Save Layout")
    ("L" persp-load-state-from-file "Load Layout")
    ("q" nil "cancel" :color blue :column nil))
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
  (persp-mode 1))
