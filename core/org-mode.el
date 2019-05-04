;;; -*- lexical-binding: t; -*-

(defvar org-drill-p nil)
(defun org-drill-wrapper ()
  (interactive)
  (unless org-drill-p
    (use-package org-drill
      :after org
      :straight nil
      :defer 1
      :commands org-drill
      :config
      (message "Loading org-drill...")
      (defun org-drill-sound ()
	(interactive)
	(if-let ((drill-sound (org-entry-get (point) "DRILL_SOUND")))
	    (start-process-shell-command "mplayer" "*sound*" (format "mplayer %s" (expand-file-name drill-sound)))
	  (read-aloud--string (org-get-heading t t t t) "word")))
      (defvar org-drill--repeat-key ?r "")
      (defun org-drill-presentation-prompt (&rest fmt-and-args)
	(let* ((item-start-time (current-time))
	       (input nil)
	       (ch nil)
	       (last-second 0)
	       (mature-entry-count (+ (length *org-drill-young-mature-entries*)
				      (length *org-drill-old-mature-entries*)
				      (length *org-drill-overdue-entries*)))
	       (status (first (org-drill-entry-status)))
	       (prompt
		(if fmt-and-args
		    (apply 'format
			   (first fmt-and-args)
			   (rest fmt-and-args))
		  (format (concat "Press key for answer, "
				  "%c=edit, %c=tags, %c=skip, %c=repeat, %c=quit.")
			  org-drill--edit-key
			  org-drill--tags-key
			  org-drill--skip-key
			  org-drill--repeat-key
			  org-drill--quit-key))))
	  (setq prompt
		(format "%s %s %s %s %s %s"
			(propertize
			 (char-to-string
			  (cond
			   ((eql status :failed) ?F)
			   (*org-drill-cram-mode* ?C)
			   (t
			    (case status
			      (:new ?N) (:young ?Y) (:old ?o) (:overdue ?!)
			      (t ??)))))
			 'face `(:foreground
				 ,(case status
				    (:new org-drill-new-count-color)
				    ((:young :old) org-drill-mature-count-color)
				    ((:overdue :failed) org-drill-failed-count-color)
				    (t org-drill-done-count-color))))
			(propertize
			 (number-to-string (length *org-drill-done-entries*))
			 'face `(:foreground ,org-drill-done-count-color)
			 'help-echo "The number of items you have reviewed this session.")
			(propertize
			 (number-to-string (+ (length *org-drill-again-entries*)
					      (length *org-drill-failed-entries*)))
			 'face `(:foreground ,org-drill-failed-count-color)
			 'help-echo (concat "The number of items that you failed, "
					    "and need to review again."))
			(propertize
			 (number-to-string mature-entry-count)
			 'face `(:foreground ,org-drill-mature-count-color)
			 'help-echo "The number of old items due for review.")
			(propertize
			 (number-to-string (length *org-drill-new-entries*))
			 'face `(:foreground ,org-drill-new-count-color)
			 'help-echo (concat "The number of new items that you "
					    "have never reviewed."))
			prompt))
	  (org-drill-sound)
	  (if (and (eql 'warn org-drill-leech-method)
		   (org-drill-entry-leech-p))
	      (setq prompt (concat
			    (propertize "!!! LEECH ITEM !!!
									    You seem to be having a lot of trouble memorising this item.
									    Consider reformulating the item to make it easier to remember.\n"
					'face '(:foreground "red"))
			    prompt)))
	  (while (memq ch '(nil org-drill--tags-key))
	    (setq ch nil)
	    (while (not (input-pending-p))
	      (let ((elapsed (time-subtract (current-time) item-start-time)))
		(message (concat (if (>= (time-to-seconds elapsed) (* 60 60))
				     "++:++ "
				   (format-time-string "%M:%S " elapsed))
				 prompt))
		(sit-for 1)))
	    (setq input (read-key-sequence nil))
	    (if (stringp input) (setq ch (elt input 0)))
	    (if (eql ch org-drill--tags-key)
		(org-set-tags-command)))
	  (case ch
	    (org-drill--quit-key nil)
	    (org-drill--edit-key 'edit)
	    (org-drill--skip-key 'skip)
	    (org-drill--repeat-key 'sound)
	    (otherwise t))))
      (defun org-drill-reschedule ()
	"Returns quality rating (0-5), or nil if the user quit."
	(let ((ch nil)
	      (input nil)
	      (next-review-dates (org-drill-hypothetical-next-review-dates))
	      (key-prompt (format "(0-5, %c=help, %c=edit, %c=tags, %c=repeat, %c=quit)"
				  org-drill--help-key
				  org-drill--edit-key
				  org-drill--tags-key
				  org-drill--repeat-key
				  org-drill--quit-key)))
	  (save-excursion
	    (while (not (memq ch (list org-drill--quit-key
				       org-drill--edit-key
				       7          ; C-g
				       ?0 ?1 ?2 ?3 ?4 ?5)))
	      (setq input (read-key-sequence
			   (if (eq ch org-drill--help-key)
			       (format "0-2 Means you have forgotten the item.
										    3-5 Means you have remembered the item.

										    0 - Completely forgot.
										    1 - Even after seeing the answer, it still took a bit to sink in.
										    2 - After seeing the answer, you remembered it.
										    3 - It took you awhile, but you finally remembered. (+%s days)
										    4 - After a little bit of thought you remembered. (+%s days)
										    5 - You remembered the item really easily. (+%s days)

										    How well did you do? %s"
				       (round (nth 3 next-review-dates))
				       (round (nth 4 next-review-dates))
				       (round (nth 5 next-review-dates))
				       key-prompt)
			     (format "How well did you do? %s" key-prompt))))
	      (cond
	       ((stringp input)
		(setq ch (elt input 0)))
	       ((and (vectorp input) (symbolp (elt input 0)))
		(case (elt input 0)
		  (up (ignore-errors (forward-line -1)))
		  (down (ignore-errors (forward-line 1)))
		  (left (ignore-errors (backward-char)))
		  (right (ignore-errors (forward-char)))
		  (prior (ignore-errors (scroll-down))) ; pgup
		  (next (ignore-errors (scroll-up)))))  ; pgdn
	       ((and (vectorp input) (listp (elt input 0))
		     (eventp (elt input 0)))
		(case (car (elt input 0))
		  (wheel-up (ignore-errors (mwheel-scroll (elt input 0))))
		  (wheel-down (ignore-errors (mwheel-scroll (elt input 0)))))))
	      (if (eql ch org-drill--tags-key)
		  (org-set-tags-command))
	      (if (eql ch org-drill--repeat-key)
		  (org-drill-sound))
	      ))
	  (cond
	   ((and (>= ch ?0) (<= ch ?5))
	    (let ((quality (- ch ?0))
		  (failures (org-drill-entry-failure-count)))
	      (unless *org-drill-cram-mode*
		(save-excursion
		  (let ((quality (if (org-drill--entry-lapsed-p) 2 quality)))
		    (org-drill-smart-reschedule quality
						(nth quality next-review-dates))))
		(push quality *org-drill-session-qualities*)
		(cond
		 ((<= quality org-drill-failure-quality)
		  (when org-drill-leech-failure-threshold
		    ;;(setq failures (if failures (string-to-number failures) 0))
		    ;; (org-set-property "DRILL_FAILURE_COUNT"
		    ;;                   (format "%d" (1+ failures)))
		    (if (> (1+ failures) org-drill-leech-failure-threshold)
			(org-toggle-tag "leech" 'on))))
		 (t
		  (let ((scheduled-time (org-get-scheduled-time (point))))
		    (when scheduled-time
		      (message "Next review in %d days"
			       (- (time-to-days scheduled-time)
				  (time-to-days (current-time))))
		      (sit-for 0.5)))))
		(org-set-property "DRILL_LAST_QUALITY" (format "%d" quality))
		(org-set-property "DRILL_LAST_REVIEWED"
				  (time-to-inactive-org-timestamp (current-time))))
	      quality))
	   ((= ch org-drill--edit-key)
	    'edit)
	   (t
	    nil))))
      )
    (setq org-drill-p t))
  (org-drill))

(use-package org
  ;; :defer 10
  :straight (org :type git :host github :repo "emacsmirror/org")
  ;; :straight org-plus-contrib
  :mode (("\\.org$" . org-mode))
  :init
  (setq org-imenu-depth 3)
  (setq org-confirm-babel-evaluate nil)
  (defvar org-babel-do-load-languages-p nil)
  :config
  ;; (defalias 'origin-org-babel-execute-src-block 'org-babel-execute-src-block)
  (defun sl/org-babel-execute-src-block (&optional orig-fun arg info params)
    (interactive "P")
    (unless org-babel-do-load-languages-p
      (use-package ob-async
	:after org
	:config (advice-add 'org-babel-execute-src-block :around 'ob-async-org-babel-execute-src-block))

      (use-package ob-rust
	:after org)

      (use-package ob-tmux
	;; Install package automatically (optional)
	:after org
	:custom
	(org-babel-default-header-args:tmux
	 '(
	   (:results . "silent")	 ;
	   ;; (:results . "output")	 ;
	   (:session . "default")	 ; The default tmux session to send code to
	   (:socket  . nil)            ; The default tmux socket to communicate with
	   ;; You can use "xterm" and "gnome-terminal".
	   ;; On mac, you can use "iterm" as well.
	   (:terminal . "iterm")))
	;; The tmux sessions are prefixed with the following string.
	;; You can customize this if you like.
	(org-babel-tmux-session-prefix "ob-")
	;; Finally, if your tmux is not in your $PATH for whatever reason, you
	;; may set the path to the tmux binary as follows:
	(org-babel-tmux-location "/usr/local/bin/tmux"))

      (org-babel-do-load-languages
       'org-babel-load-languages
       '((emacs-lisp . t)
	 (dot . t)
	 (sql . t)
	 (ruby . t)
	 (R . t)
	 (org . t)
	 (screen . t)
	 (shell . t)
	 (rust . t)
	 (plantuml . t)
	 (C . t)
	 (js . t)))
      (setq org-babel-do-load-languages-p t))
    (funcall orig-fun arg info params))

  (advice-add 'org-babel-execute-src-block :around 'sl/org-babel-execute-src-block)
  ;; (org-babel-do-load-languages
  ;;  'org-babel-load-languages
  ;;  '((emacs-lisp . t)
  ;;    (dot . t)
  ;;    (sql . t)
  ;;    (ruby . t)
  ;;    (R . t)
  ;;    (org . t)
  ;;    (screen . t)
  ;;    (shell . t)
  ;;    (rust . t)
  ;;    (plantuml . t)
  ;;    (C . t)
  ;;    (js . t)))

  (setq org-capture-templates
	'(
	  ("v"
	   "Vocabulary"
	   entry
	   (file "~/org-modes/flashcards.org")
	   "* %i%^{prompt} :drill: \n\nTranslate\n\n** Answer\n\n[[file:~/org-modes/images/%\\1.png]]\n\n** Image Source\n\n#+begin_src shell\ntest -f ~/org-modes/images/%\\1.png || wget -O ~/org-modes/images/%\\1.png \"%\\1%?\"\n#+end_src")
	  ("s"
	   "Speaking English"
	   entry
	   (file "~/org-modes/flashcards.org")
	   "* %i%^{sentence} :drill:speaking:\n:PROPERTIES:\n:DRILL_SOUND: ~/org-modes/english/%^{sound}\n:END:\n\nSpeaking loudly man\n\n** Answer\n\n[[file:~/org-modes/english/%\\2]]\n\n** Source\n\n#+begin_src shell\ntest -f ~/org-modes/english/%\\2 || wget -O ~/org-modes/english/%\\2 \"%^{link}\"\n#+end_src")
	  ("a"
	   "Appointment"
	   entry
	   (file+headline "~/org-modes/personal.org" "Appointments")
	   "* TODO %?\n:PROPERTIES:\n\n:END:\nDEADLINE: %^T \n %i\n"
	   )
	  ("l"
	   "learn"
	   entry
	   (file "~/org-modes/learn.org")
	   "* %? :drill:\n"
	   )
	  ("c"
	   "Reading List"
	   entry
	   (file+headline "~/org-modes/personal.org" "Reading List")
	   "* TODO %?\n")
	  ("p"
	   "Notes"
	   entry
	   (file "~/org-modes/notes.org")
	   "* %:description\n\nSource: %:link\nCaptured On:%U\n\n%:initial\n\n")
	  ("t"
	   "Token"
	   plain
	   (file "/tmp/token.org")
	   "%:initial"
	   :immediate-finish
	   :prepend)
	  ("L"
	   "Notes"
	   entry
	   (file "~/org-modes/notes.org")
	   "* %:description\n\nSource: %:link\nCaptured On:%U\n\n%:initial\n\n"
	   :immediate-finish
           :prepend)
	  ("E"
	   "Employment Hero Task"
	   entry
	   (file "~/org-modes/employmenthero.org")
	   "* TODO %:initial\n\nSource: %:link\nCaptured On:%U\n\n")
	  ("e"
	   "Employment Hero Task"
	   entry
	   (file "~/org-modes/employmenthero.org")
	   "* TODO %?")))

  ;; org-drill
  (setq org-drill-maximum-items-per-session 40)
  (setq org-drill-maximum-duration 30)   ; 30 minutes
  (setq org-drill-learn-fraction 0.1)
  (setq org-drill-spaced-repetition-algorithm 'sm2)
  ;; (setq org-plantuml-jar-path "~/org-modes/plantuml.jar")
  (setq org-plantuml-jar-path "~/org-modes/plantuml.beta.jar")

  ;; customize
  ;; (defun jws/org-protocol-capture-p ()
  ;;   "Return true if this capture was initiated via org-protocol."
  ;;   (equal (buffer-name (org-capture-get :original-buffer)) " *server*"))

  ;; (defun jws/org-capture-after-finalize ()
  ;;   "Delete frame if capture was initiated via org-protocol"
  ;;   (message "jws/org-capture-after-finalize %s" (string= "fcapture" (cdr (assoc 'name (frame-parameters)))))
  ;;   (message "%s" (cdr (assoc 'name (frame-parameters))))
  ;;   (message "%s" (jws/org-protocol-capture-p))
  ;;   ;; (if (jws/org-protocol-capture-p) (delete-frame))
  ;;   )

  ;; (defun jws/org-capture-initialize ()
  ;;   "Make sure frame has only one window if capture was initiated via org-protocol"
  ;;   (if (jws/org-protocol-capture-p) (delete-other-windows)))

  ;; (add-hook 'org-capture-mode-hook 'jws/org-capture-initialize)
  ;; (add-hook 'org-capture-after-finalize-hook 'jws/org-capture-after-finalize)

  )

(use-package evil-org
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (setq evil-org-use-additional-insert t
	evil-org-key-theme `(textobjects
			     navigation
			     additional
			     calendar
			     todo)
	org-image-actual-width 600
	org-babel-temporary-directory "/tmp")
  (evil-org-agenda-set-keys))

(defun org-babel-execute:tmux (body params)
  "Send a block of code via tmux to a terminal using Babel.
\"default\" session is used when none is specified.
Argument BODY the body of the tmux code block.
Argument PARAMS the org parameters of the code block."
  (message "Sending source code block to interactive terminal session...")
  (save-window-excursion
    (let* ((org-session (cdr (assq :session params)))
	   (terminal (cdr (assq :terminal params)))
	   (socket (cdr (assq :socket params)))
	   (socket (when socket (expand-file-name socket)))
	   (ob-session (ob-tmux--from-org-session org-session socket))
	   (log-file (expand-file-name (concat "~/tmux." org-session ".log")))
	   (log-file-before (expand-file-name (concat "/tmp/tmux." org-session ".log.before")))
	   (session-alive (ob-tmux--session-alive-p ob-session))
	   (window-alive (ob-tmux--window-alive-p ob-session)))
      ;; Create tmux session and window if they do not yet exist
      (unless session-alive (ob-tmux--create-session ob-session))
      (unless window-alive (ob-tmux--create-window ob-session))
      ;; Start terminal window if the session does not yet exist
      (unless session-alive
	(ob-tmux--start-terminal-window ob-session terminal))
      ;; Wait until tmux window is available
      (while (not (ob-tmux--window-alive-p ob-session)))
      ;; Disable window renaming from within tmux
      (ob-tmux--disable-renaming ob-session)
      (message "Store log file")
      (copy-file log-file log-file-before t)
      (ob-tmux--send-body
       ob-session (org-babel-expand-body:generic body params)))))

(use-package org-tree-slide
  :commands (org-tree-slide-mode org-tree-slide-skip-done-toggle)
  :config (org-tree-slide-simple-profile))

(use-package org-pomodoro :after org :commands org-pomodoro)

(defun org-in-any-block-p ()
  "Return non-nil if the point is in any Org block.
The Org block can be *any*: src, example, verse, etc., even any
Org Special block.
This function is heavily adapted from `org-between-regexps-p'."
  (save-match-data
    (let ((pos (point))
	  (case-fold-search t)
	  (block-begin-re "^[[:blank:]]*#\\+begin_\\(?1:.+?\\)\\(?: .*\\)*$")
	  (limit-up (save-excursion (outline-previous-heading)))
	  (limit-down (save-excursion (outline-next-heading)))
	  beg end)
      (save-excursion
	;; Point is on a block when on BLOCK-BEGIN-RE or if
	;; BLOCK-BEGIN-RE can be found before it...
	(and (or (org-in-regexp block-begin-re)
		 (re-search-backward block-begin-re limit-up :noerror))
	     (setq beg (match-beginning 0))
	     ;; ... and BLOCK-END-RE after it...
	     (let ((block-end-re (concat "^[[:blank:]]*#\\+end_"
					 (match-string-no-properties 1)
					 "\\( .*\\)*$")))
	       (goto-char (match-end 0))
	       (re-search-forward block-end-re limit-down :noerror))
	     (> (setq end (match-end 0)) pos)
	     ;; ... without another BLOCK-BEGIN-RE in-between.
	     (goto-char (match-beginning 0))
	     (not (re-search-backward block-begin-re (1+ beg) :noerror))
	     ;; Return value.
	     (cons beg end))))))


(defun org-split-block ()
  "Sensibly split the current Org block at point.
(1) Point in-between a line
    #+begin_src emacs-lisp             #+begin_src emacs-lisp
    (message▮ \"one\")                   (message \"one\")
    (message \"two\")          -->       #+end_src
    #+end_src                          ▮
				       #+begin_src emacs-lisp
				       (message \"two\")
				       #+end_src
(2) Point at EOL
    #+begin_src emacs-lisp             #+begin_src emacs-lisp
    (message \"one\")▮                   (message \"one\")
    (message \"two\")          -->       #+end_src
    #+end_src                          ▮
				       #+begin_src emacs-lisp
				       (message \"two\")
				       #+end_src
(3) Point at BOL
    #+begin_src emacs-lisp             #+begin_src emacs-lisp
    (message \"one\")                    (message \"one\")
    ▮(message \"two\")          -->      #+end_src
    #+end_src                          ▮
				       #+begin_src emacs-lisp
				       (message \"two\")
				       #+end_src
"
  (interactive)
  (if (org-in-any-block-p)
      (save-match-data
	(save-restriction
	  (widen)
	  (let ((case-fold-search t)
		(at-bol (bolp))
		block-start
		block-end)
	    (save-excursion
	      (re-search-backward "^\\(?1:[[:blank:]]*#\\+begin_.+?\\)\\(?: .*\\)*$" nil nil 1)
	      (setq block-start (match-string-no-properties 0))
	      (setq block-end (replace-regexp-in-string
			       "begin_" "end_" ;Replaces "begin_" with "end_", "BEGIN_" with "END_"
			       (match-string-no-properties 1))))
	    ;; Go to the end of current line, if not at the BOL
	    (unless at-bol
	      (end-of-line 1))
	    (insert (concat (if at-bol "" "\n")
			    block-end
			    "\n\n"
			    block-start
			    (if at-bol "\n" "")))
	    ;; Go to the line before the inserted "#+begin_ .." line
	    (beginning-of-line (if at-bol -1 0)))))
    (message "Point is not in an Org block")))
