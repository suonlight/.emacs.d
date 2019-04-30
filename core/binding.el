;;; -*- lexical-binding: t; -*-
(use-package general)

(general-define-key
 :keymaps 'process-menu-mode-map
 :states 'normal
 "d"       #'process-menu-delete-process
 "s-<f12>" #'quit-window
 "q"       #'quit-window)

(general-define-key
 :keymaps 'racer-help-mode-map
 :states 'normal
 "q"       #'quit-window)

(general-define-key
 :keymaps 'global-map
 :states '(normal visual insert emacs)
 "s-<mouse-1>"  #'dumb-jump-go
 "s-k"         #'awesome-tab-forward-tab
 "s-j"         #'awesome-tab-backward-tab
 "s-}"         #'awesome-tab-forward-tab
 "s-{"         #'awesome-tab-backward-tab
 "s-]"         #'awesome-tab-forward-group
 "s-["         #'awesome-tab-backward-group
 "s-b"         #'ivy-switch-buffer
 "s-`"         #'multi-libvterm-projectile
 "<f10>"       #'toggle-maximize-buffer
 "<f11>"       #'(sl/awesome-tab-group-hydra/body :which-key "layouts")
 "<f12>"       #'prodigy
 "s-<f12>"     #'list-processes-other-window
 "C-p"         #'counsel-projectile-find-file)

(general-define-key
 :keymaps 'global-map
 :states '(normal visual)
 "gy"          (general-simulate-key "yyPgcc" :state 'normal))

;; (global-set-key (kbd "M-v") #'yank)

;; (define-key isearch-mode-map (kbd "M-v") 'isearch-yank-kill)
;; (define-key evil-ex-search-keymap (kbd "M-v") 'isearch-yank-kill)

(general-define-key
 :keymaps 'ivy-minibuffer-map
 "C-f m" #'counsel-insert-models
 "C-f c" #'counsel-insert-controllers
 "C-f p" #'counsel-insert-policies
 "C-f t" #'counsel-insert-tests
 "C-f s" #'counsel-insert-services
 "C-v" (general-simulate-key "M-o v")
 "C-s" (general-simulate-key "M-o s"))

;; (define-key ivy-minibuffer-map (kbd "C-m") '(lambda () (insert "models")))

(general-define-key
 :states '(insert emacs)
 :keymaps 'emmet-mode-keymap
 "TAB"   #'emmet-expand-line
 "<tab>" #'emmet-expand-line
 )

(general-define-key
 :states '(normal visual)
 "s"   #'evil-avy-goto-word-or-subword-1
 "gj"  #'evil-avy-goto-char)

(general-create-definer leader-define-key
  ;; :prefix my-leader
  ;; or without a variable
  :states '(normal visual)
  :prefix ",")

;; (define-key org-present-mode-keymap (kbd "C-p") #'org-present-prev)
;; (define-key org-present-mode-keymap (kbd "C-n") #'org-present-next)
;; (general-define-key
;;  :keymaps 'org-present-mode-keymap
;;  :states '(normal visual)
;;  "p" #'org-present-prev
;;  "n" #'org-present-next
;;  "q" #'org-present-quit)

(leader-define-key rust-mode-map
  "gG" #'racer-find-definition-other-window
  "gg" #'racer-find-definition
  "gb" #'pop-tag-mark
  "hh" #'racer-describe

  "c." #'cargo-process-repeat
  "cC" #'cargo-process-clean
  "cX" #'cargo-process-run-example
  "cc" #'cargo-process-build
  "cd" #'cargo-process-doc
  "cD" #'cargo-process-doc-open
  "ce" #'cargo-process-bench
  "cf" #'cargo-process-fmt
  "ci" #'cargo-process-init
  "cl" #'cargo-process-clippy
  "cn" #'cargo-process-new
  "co" #'cargo-process-current-file-tests
  "cs" #'cargo-process-search
  "ct" #'cargo-process-current-test
  "cu" #'cargo-process-update
  "cx" #'cargo-process-run
  "cv" #'cargo-process-check
  "tb" #'cargo-process-test)

(leader-define-key org-tree-slide-mode-map
  :states '(normal visual)
  "e" #'org-tree-slide-slide-in-effect-toggle
  "p" #'org-tree-slide-move-previous-tree
  "n" #'org-tree-slide-move-next-tree
  "c" #'org-tree-slide-content
  "q" #'org-tree-slide-mode)

(general-define-key
 :states '(normal visual)
 :keymaps 'org-mode-map
 "s-RET" #'org-split-block
 "-"     #'org-cycle-list-bullet)

(leader-define-key org-mode-map
  "'"          #'org-edit-special
  ","          #'org-babel-execute-src-block
  "K"          #'org-shiftup
  "J"          #'org-shiftdown
  "H"          #'org-shiftleft
  "L"          #'org-shiftright
  "r"          #'org-drill-sound
  "l"          #'org-drill-wrapper
  "bs"         #'org-split-block
  "dT"         #'org-time-stamp-inactive
  "dd"         #'org-deadline
  "ds"         #'org-schedule
  "dt"         #'org-time-stamp
  "Cp"         #'org-pomodoro
  "Ci"         #'org-clock-in
  "Co"         #'org-clock-out
  "Cg"         #'org-clock-goto
  "CR"         #'org-clock-report
  "Cc"         #'org-clock-cancel
  "Cd"         #'org-clock-display
  )
(leader-define-key emacs-lisp-mode-map
  "er" #'eval-region
  "eb" #'eval-buffer
  "ee" #'eval-expression)
(leader-define-key ruby-mode-map
  "r"  #'(:ignore t :which-key "refactor")
  "x"  #'(:ignore t :which-key "text")
  "rt" #'sl/ruby-copy-generated-spec-at-point
  "'" #'inf-ruby-console-auto
  "x{" #'ruby-toggle-block
  "x'" #'ruby-toggle-string-quotes)
(leader-define-key term-mode-map
  "n" #'multi-term-next
  "p" #'multi-term-prev
  "c" #'multi-term)

(leader-define-key ruby-test-mode-map
  "gG" #'dumb-jump-go-other-window
  "gg" #'dumb-jump-go
  "gb" #'dumb-jump-back
  "gx" #'dumb-jump-go-prefer-external
  "gz" #'dumb-jump-go-prefer-external-other-window

  "tb" #'ruby-test-run
  "tt" #'ruby-test-run-at-point)

(leader-define-key js2-mode-map
  "gG" #'dumb-jump-go-other-window
  "gg" #'dumb-jump-go
  "gb" #'dumb-jump-back
  "gx" #'dumb-jump-go-prefer-external
  "gz" #'dumb-jump-go-prefer-external-other-window
  "I'" #'run-import-js
  "Ii" #'import-js-import
  "If" #'import-js-fix)

(general-define-key
 :states '(normal visual insert emacs)
 "C-h"         #'evil-window-left
 "C-j"         #'evil-window-down
 "C-k"         #'evil-window-up
 "C-l"         #'evil-window-right
 ;; "C-p"         #'counsel-projectile-find-file
 ;; "C-p"         #'counsel-fzf
 "C-TAB"       #'yas-expand
 "s-s"         #'save-buffer
 "s-w"         #'kill-this-buffer
 "s-0"         #'awesome-tab-switch-group-to-0
 "s-1"         #'awesome-tab-switch-group-to-1
 "s-2"         #'awesome-tab-switch-group-to-2
 "s-3"         #'awesome-tab-switch-group-to-3
 "s-4"         #'awesome-tab-switch-group-to-4
 "s-5"         #'awesome-tab-switch-group-to-5
 "s-6"         #'awesome-tab-switch-group-to-6
 "s-7"         #'awesome-tab-switch-group-to-7
 "s-8"         #'awesome-tab-switch-group-to-8
 "s-9"         #'awesome-tab-switch-group-to-9)

(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "?"   #'counsel-descbinds
 "/"   #'counsel-rg
 "*"   #'counsel-rg-thing-at-point
 "TAB" #'evil-switch-to-windows-last-buffer
 "SPC" #'counsel-M-x
 ;; "'"   #'multi-term-toggle
 "'"   #'multi-libvterm-dedicated-toggle

 ;; help
 "h"          #'(:ignore t :which-key "help")
 "hv"         #'counsel-describe-variable
 "hf"         #'counsel-describe-function
 "hk"         #'describe-key
 "hz"         #'toggle-frame-fullscreen
 "hm"         #'benchmark-init/show-durations-tree

 ;; projects
 "p"          #'(:ignore t :which-key "projects")
 "pSPC"       #'counsel-projectile
 ;; jp'"         #'toggle-shell
 "p'"         #'multi-libvterm-projectile
 "p!"         #'projectile-run-shell-command-in-root
 "p%"         #'projectile-replace-regexp
 "p&"         #'projectile-run-async-shell-command-in-root
 "pD"         #'projectile-dired
 "pF"         #'projectile-find-file-dwim
 "pG"         #'projectile-regenerate-tags
 "pI"         #'projectile-invalidate-cache
 "pR"         #'projectile-replace
 "pT"         #'projectile-test-project
 "pa"         #'projectile-toggle-between-implementation-and-test
 "pb"         #'counsel-projectile-switch-to-buffer
 "pc"         #'projectile-compile-project
 "pd"         #'counsel-projectile-find-dir
 "pe"         #'projectile-edit-dir-locals
 "pf"         #'counsel-projectile-find-file
 ;; "pf"         #'counsel-fzf
 "orm"        #'counsel-insert-models
 "orc"        #'counsel-insert-controllers
 "orp"        #'counsel-insert-policies
 "pg"         #'projectile-find-tag
 "pk"         #'projectile-kill-buffers
 ;; "po"         #'org-projectile/goto-todos
 "pp"         #'counsel-projectile-switch-project
 ;; "pl"         #'ivy-persp-switch-project
 "pr"         #'projectile-recentf
 ;; "pt"         #'neotree-find-project-root
 "pv"         #'projectile-vc

 ;; Layout
 ;; "l"          #'(sl/persp-hydra/body :which-key "layouts")
 "l"          #'(sl/awesome-tab-group-hydra/body :which-key "layouts")
 ;; Files
 "f"          #'(:ignore t :which-key "files")
 "fyC"        #'projectile-copy-file-path-with-line-column
 "fyD"        #'projectile-copy-directory-path
 "fyL"        #'projectile-copy-file-path-with-line
 "fyY"        #'projectile-copy-file-path
 "fyc"        #'copy-file-path-with-line-column
 "fyd"        #'copy-directory-path
 "fyl"        #'copy-file-path-with-line
 "fyy"        #'copy-file-path
 "fyN"        #'copy-file-name-base
 "fyn"        #'copy-file-name

 "fL"         #'counsel-locate
 "fS"         #'evil-write-all
 "fR"         #'rename-buffer
 "fC"         #'copy-this-file
 "fb"         #'counsel-bookmark
 "fed"        #'open-config-file
 "ff"         #'counsel-find-file
 "fg"         #'rgrep
 "fh"         #'hexl-find-file
 "fj"         #'dired-jump
 "fl"         #'find-file-literally
 "fr"         #'counsel-recentf
 "fs"         #'save-buffer

 ;; Buffers
 "b"          #'(:ignore t :which-key "buffers")
 "bi"         #'ibuffer
 "bb"         #'ivy-switch-buffer
 ;; "bb"         #'sl/switch-buffer
 "bm"         #'view-echo-area-messages
 ;; "bn"         #'next-buffer
 ;; "bp"         #'previous-buffer
 "b0"         #'awesome-tab-select-beg-tab
 "b$"         #'awesome-tab-select-end-tab
 "bn"         #'awesome-tab-forward-tab
 "bp"         #'awesome-tab-backward-tab
 "bj"         #'awesome-tab-select-visible-tab
 "b<"         #'awesome-tab-move-current-tab-to-left
 "b>"         #'awesome-tab-move-current-tab-to-right
 "bw"         #'read-only-mode
 "bd"         #'kill-this-buffer
 "bs"         #'open-scratch-buffer

 ;; Window
 "w"          #'(:ignore t :which-key "windows")
 "w-"         #'evil-window-split
 "w|"         #'evil-window-vsplit
 "w="         #'balance-windows-area
 "wF"         #'make-frame
 "wH"         #'evil-window-move-far-left
 "wJ"         #'evil-window-move-very-bottom
 "wK"         #'evil-window-move-very-top
 "wL"         #'evil-window-move-far-right
 "wM"         #'ace-swap-window
 ;; "wS"         #'split-window-below-and-focus
 "wU"         #'winner-redo
 ;; "wV"         #'split-window-right-and-focus
 ;; "wW"         #'ace-window
 "wc"         #'evil-window-delete
 "wC"         #'centered-window-mode
 "wf"         #'follow-mode
 "wh"         #'evil-window-left
 "wj"         #'evil-window-down
 "wk"         #'evil-window-up
 "wl"         #'evil-window-right
 "wm"         #'toggle-maximize-buffer
 "wo"         #'other-frame
 "ws"         #'evil-window-split
 "wu"         #'winner-undo
 "wv"         #'evil-window-vsplit
 "ww"         #'other-window
 "wx"         #'kill-buffer-and-window

 ;; errors
 "e"          #'(:ignore t :which-key "errors")
 "eN"         #'flycheck-previous-error
 "eS"         #'flycheck-set-checker-executable
 "eb"         #'flycheck-buffer
 "ec"         #'flycheck-clear
 "ef"         #'rubocop-autocorrect-current-file
 "eh"         #'flycheck-describe-checker
 "el"         #'flycheck-list-errors
 "en"         #'flycheck-next-error
 "ep"         #'flycheck-previous-error
 "es"         #'flycheck-select-checker
 "ev"         #'flycheck-verify-setup
 "ex"         #'flycheck-explain-error-at-point
 "ey"         #'flycheck-copy-errors-as-kill

 ;; jump
 "j"          #'(:ignore t :which-key "jump")
 "jD"         #'dired-jump-other-window
 "jb"         #'avy-pop-mark
 "jd"         #'dired-jump
 "jf"         #'find-function
 "jj"         #'evil-avy-goto-char-timer
 "jr"         #'avy-resume
 "ji"         #'counsel-imenu
 "jl"         #'evil-avy-goto-line
 "jn"         #'sp-newline
 "jo"         #'open-line
 "jq"         #'dumb-jump-quick-look
 "js"         #'sp-split-sexp
 "jv"         #'find-variable
 "jw"         #'evil-avy-goto-word-or-subword-1

 ;; registers
 "r"          #'(:ignore t :which-key "registers")
 "rl"         #'ivy-resume
 "re"         #'counsel-evil-registers

 ;; Quit
 "q"          #'(:ignore t :which-key "quit")
 "qq"         #'save-buffers-kill-terminal
 "qe"         #'server-edit

 ;; Search
 "s"          #'(:ignore t :which-key "search/yas")
 "ss"         #'swiper
 "sf"         #'counsel-rg-from-current-directory
 "sh"         #'highlight-symbol-at-point
 "sc"         #'unhighlight-regexp-all
 "sn"         #'yas-new-snippet
 "si"         #'yas-insert-snippet
 "s/"         #'yas-visit-snippet-file

 ;; Org
 "a"         #'(:ignore t :which-key "applications")
 "ao"        #'(:ignore t :which-key "org")
 "ao#"       #'org-agenda-list-stuck-projects
 "ao/"       #'org-occur-in-agenda-files
 ;; "aoC"       #'Prefix Command
 "aoa"       #'org-agenda-list
 ;; "aob"       #'org-brain-visualize
 "aoc"       #'org-capture
 "aoe"       #'org-store-agenda-views
 ;; "aof"       #'Prefix Command
 "aol"       #'org-store-link
 "aom"       #'org-tags-view
 "aoo"       #'org-agenda
 ;; "aop"       #'org-projectile/capture
 "aos"       #'org-search-view
 "aot"       #'org-todo-list
 "ap"        #'list-processes-other-window

 ;; Themes
 "T"         #'(:ignore t :which-key "themes")
 "Ts"        #'counsel-load-theme
 "Tn"        #'next-cycle-theme

 ;; Git
 "g"         #'(:ignore t :which-key "git")
 "gs"        #'magit-status
 "gt"        #'sl/git-timemachine-on
 "gb"        #'magit-blame-addition
 "gM"        #'git-messenger:popup-message
 "gl"        #'(:ignore t :which-key "link")
 "gll"       #'git-link
 "glc"       #'git-link-commit
 "gh"        #'(:ignore t :which-key "hub")
 "ghr"       #'magithub-issue-refresh
 "ghp"       #'magithub-pull-visit

 ;; Text
 "x"         #'(:ignore t :which-key "text")
 "xfg"       #'copy-as-format-github
 "xfh"       #'copy-as-format-html
 "xfm"       #'copy-as-format-markdown
 "xfo"       #'copy-as-format-org-mode
 "xfs"       #'copy-as-format-slack
 "xgt"       #'google-translate-at-point
 "xs"        #'read-aloud-this

 ;; Others
 "as"        #'(:ignore t :which-key "shell/term")
 "asv"       #'multi-libvterm
 "ast"       #'toggle-tmux)
