;;; -*- lexical-binding: t; -*-
(use-package general)

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'flycheck-error-list-mode-map
 "q" #'quit-window)

(general-define-key
 :keymaps 'inf-ruby-mode-map
 :states '(normal visual)
 "o"       #'evil-insert-resume
 "i"       #'evil-insert-resume)

(general-define-key
 :keymaps 'comint-mode-map
 :states 'insert
 "C-p"     #'comint-previous-input
 "C-n"     #'comint-next-input)

(general-define-key
 :keymaps 'process-menu-mode-map
 :states 'normal
 "d"       #'process-menu-delete-process
 "s-<f12>" #'quit-window
 "q"       #'quit-window)


(general-define-key
 :keymaps 'osx-dictionary-mode-map
 :states 'normal
 "r"       #'osx-dictionary-read-word
 "o"       #'osx-dictionary-open-dictionary\.app
 "s"       #'osx-dictionary-search-input
 "q"       #'osx-dictionary-quit)

(general-define-key
 :keymaps 'racer-help-mode-map
 :states 'normal
 "q"       #'quit-window)

(general-define-key
 :keymaps 'global-map
 :states '(normal visual)
 "C-p"         #'counsel-projectile-find-file)

(general-define-key
 :keymaps 'ruby-mode-map
 :states '(normal visual insert emacs)
 "s-<mouse-1>"  #'dumb-jump-go)

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

;; (general-define-key
;;  :states '(insert emacs)
;;  :keymaps 'emmet-mode-keymap
;;  "TAB"   #'emmet-expand-line
;;  "<tab>" #'emmet-expand-line
;;  )

(general-define-key
 :states '(normal visual)
 "s"   #'evil-avy-goto-word-or-subword-1)

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
  "b"          #'(:ignore t :which-key "org-babel")
  "b."         #'hydra-org-babel/body
  "bs"         #'org-split-block
  "ba"         #'org-babel-sha1-hash
  "bb"         #'org-babel-execute-buffer
  "bc"         #'org-babel-check-src-block
  "bd"         #'org-babel-demarcate-block
  "be"         #'org-babel-execute-maybe
  "bf"         #'org-babel-tangle-file
  "bg"         #'org-babel-goto-named-src-block
  "bi"         #'org-babel-lob-ingest
  "bI"         #'org-babel-view-src-block-info
  "bj"         #'org-babel-insert-header-arg
  "bl"         #'org-babel-load-in-session
  "bn"         #'org-babel-next-src-block
  "bo"         #'org-babel-open-src-block-result
  "bp"         #'org-babel-previous-src-block
  "br"         #'org-babel-goto-named-result
  "bS"         #'org-babel-execute-subtree
  "bt"         #'org-babel-tangle
  "bu"         #'org-babel-goto-src-block-head
  "bv"         #'org-babel-expand-src-block
  "bx"         #'org-babel-do-key-sequence-in-edit-buffer
  "bz"         #'org-babel-switch-to-session
  "bZ"         #'org-babel-switch-to-session-with-code
  "bk"         #'org-babel-remove-result-one-or-many
  "d"          #'(:ignore t :which-key "date")
  "dT"         #'org-time-stamp-inactive
  "dd"         #'org-deadline
  "ds"         #'org-schedule
  "dt"         #'org-time-stamp
  "C"          #'(:ignore t :which-key "clock")
  "Cp"         #'org-pomodoro
  "Ci"         #'org-clock-in
  "Co"         #'org-clock-out
  "Cg"         #'org-clock-goto
  "CR"         #'org-clock-report
  "Cc"         #'org-clock-cancel
  "Cd"         #'org-clock-display
  "p"          #'(:ignore t :which-key "presentation")
  "p'"         #'org-tree-slide-mode
  "ps"         #'org-tree-slide-skip-done-toggle
  )
(leader-define-key emacs-lisp-mode-map
  "er" #'eval-region
  "eb" #'eval-buffer
  "ee" #'eval-expression)
(leader-define-key ruby-mode-map
  "'"    #'inf-ruby-console-auto
  "d"    #'(:ignore t :which-key "debugger")
  "d'"   #'dap-mode
  "r"    #'(:ignore t :which-key "refactor")
  "rt"   #'sl/ruby-copy-generated-spec-at-point
  "x"    #'(:ignore t :which-key "text")
  "x{"   #'ruby-toggle-block
  "x'"   #'ruby-toggle-string-quotes)
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

;; (leader-define-key js2-mode-map
(leader-define-key js-mode-map
  "d"  #'(:ignore t :which-key "debugger")
  "d'" #'dap-mode
  "gG" #'dumb-jump-go-other-window
  "gg" #'dumb-jump-go
  "gb" #'dumb-jump-back
  "gx" #'dumb-jump-go-prefer-external
  "gz" #'dumb-jump-go-prefer-external-other-window
  "I'" #'run-import-js
  "Ii" #'import-js-import
  "If" #'import-js-fix)

(general-define-key
 :keymaps 'global-map
 :states '(normal visual insert emacs)
 "s-k"         #'centaur-tabs-forward-tab
 "s-j"         #'centaur-tabs-backward-tab
 "s-}"         #'persp-next
 "s-{"         #'persp-prev
 "s-]"         #'persp-next
 "s-["         #'persp-prev
 "s-b"         #'counsel-projectile-switch-to-buffer
 "s-`"         #'evil-multi-libvterm-projectile
 "s-<S-w>"     #'evil-window-delete
 "<f10>"       #'toggle-maximize-buffer
 "<f11>"       #'(sl/persp-hydra/body :which-key "layouts")
 "<f12>"       #'prodigy-as-default-layout
 "s-<f12>"     #'list-processes-other-window)

(general-define-key
 :states '(normal visual insert emacs motion)
 "C-h"         #'evil-window-left
 "C-j"         #'evil-window-down
 "C-k"         #'evil-window-up
 "C-l"         #'evil-window-right
 ;; "C-p"         #'counsel-projectile-find-file
 ;; "C-p"         #'counsel-fzf
 "C-TAB"       #'yas-expand
 "s-s"         #'save-buffer
 "s-w"         #'kill-this-buffer
 "s-0"         #'persp-switch-to-0
 "s-1"         #'persp-switch-to-1
 "s-2"         #'persp-switch-to-2
 "s-3"         #'persp-switch-to-3
 "s-4"         #'persp-switch-to-4
 "s-5"         #'persp-switch-to-5
 "s-6"         #'persp-switch-to-6
 "s-7"         #'persp-switch-to-7
 "s-8"         #'persp-switch-to-8
 "s-9"         #'persp-switch-to-9)

(general-define-key
 :states '(normal visual)
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
 "p'"         #'evil-multi-libvterm-projectile
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
 "pl"         #'ivy-persp-switch-project
 "pr"         #'projectile-recentf
 ;; "pt"         #'neotree-find-project-root
 "pv"         #'projectile-vc

 ;; Layout
 "l"          #'(sl/persp-hydra/body :which-key "layouts")
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
 "b0"         #'centaur-tabs-select-beg-tab
 "b$"         #'centaur-tabs-select-end-tab
 "bn"         #'centaur-tabs-forward-tab
 "bp"         #'centaur-tabs-backward-tab
 "bj"         #'centaur-tabs-select-visible-tab
 "b<"         #'centaur-tabs-move-current-tab-to-left
 "b>"         #'centaur-tabs-move-current-tab-to-right
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
 "w2"         #'split-window-vertically-2
 "w3"         #'split-window-vertically-3
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
 "qs"         #'save-buffers-kill-emacs
 "qf"         #'delete-frame

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
 "ac"        #'calendar
 "ad"        #'projectile-dired

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
 "xi"        #'(:ignore t :which-key "string-inflection")
 "xi-"       #'string-inflection-kebab-case
 "xiC"       #'string-inflection-camelcase
 "xiU"       #'string-inflection-upcase
 "xi_"       #'string-inflection-underscore
 "xic"       #'string-inflection-lower-camelcase
 "xik"       #'string-inflection-kebab-case
 "xiu"       #'string-inflection-underscore
 "xf"        #'(:ignore t :which-key "format")
 "xfg"       #'copy-as-format-github
 "xfh"       #'copy-as-format-html
 "xfm"       #'copy-as-format-markdown
 "xfo"       #'copy-as-format-org-mode
 "xfs"       #'copy-as-format-slack
 "xgt"       #'google-translate-at-point
 "xwd"       #'osx-dictionary-search-word-at-point
 "xs"        #'read-aloud-this

 ;; Frame
 "F"         #'(:ignore t :which-key "frame")
 "Fb"        #'switch-to-buffer-other-frame
 "Fd"        #'delete-frrame
 "FD"        #'delete-other-frrame
 "FO"        #'dired-other-frame
 "Fo"        #'other-frame
 "Fn"        #'make-frame
 "Ff"        #'find-file-other-frame

 ;; Others
 "as"        #'(:ignore t :which-key "shell/term")
 "asv"       #'multi-libvterm
 "ast"       #'toggle-tmux)
