;; parker's emacs configuration

;; On macOS, I typically run `emacs-plus` with some options
;; brew install emacs-plus@30 --with-savchenkovaleriy-big-sur-3d-icon
;; --with-savchenkovaleriy-big-sur-3d-icon for pretty icon

;;; GENERAL / STARTUP
;; setup MELPA
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))

;; setup use-package
(when (< emacs-major-version 29)
  (unless (package-installed-p 'use-package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package)))

;; don't store UI configured things in this file
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

;; Don't show the splash screen upon startup, maybe I should replace this with something nicer...
(setq inhibit-startup-message t)

;; backup files & cleanup
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

;;; KEYBINDINGS
;; Enable Evil Mode
(unless (package-installed-p 'evil)
  (package-install 'evil))
(require 'evil)
(evil-mode 1)

(use-package evil-collection ;; vi keys in other modes (org, etc).
  :ensure t
  :after evil
  :config
  (evil-collection-init))

;;make C-g better
;; taken from https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/#h:1e468b2a-9bee-4571-8454-e3f5462d9321
(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))
(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)

;;; MINI BUFFER
(use-package vertico ;; display minibuffer commands verically
  :ensure t
  :hook (after-init . vertico-mode))
(use-package marginalia ;; command descriptions
  :ensure t
  :hook (after-init . marginalia-mode))
(use-package orderless ;; don't need to remember any command orders
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides nil))
(use-package savehist ;; persist history across sessions
  :ensure nil ; built in
  :hook (after-init . savehist-mode))

;;; APPEARANCE
(setq frame-resize-pixelwise t) ; get rid of annoying gaps
(tool-bar-mode -1) ;; Disable toolbar at the top of the screen
(scroll-bar-mode -1) ;; Disable scroll bars (might turn these back on)
(menu-bar-mode 1) ;; Keep the menu bar enabled on MacOS (might disable this in other envs)
(global-display-line-numbers-mode 1) ;; enable line numbers
(setq display-line-numbers-type 'visual) ;; don't account for folded code, this is nce for vim motions
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)) ;; transparent titlebar
(add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; dark mode
(use-package ef-themes
  :ensure t
  :config
  (load-theme 'ef-owl :no-confirm))

;; use preferred fonts
;; Install JetBrains Mono font: brew install --cask font-jetbrains-mono
(let ((mono-spaced-font "JetBrains Mono")
      (proportionately-spaced-font "Sans"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 175)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

;; nerd icons
;; note: must run `M-x nerd-icons-install-fonts` once
(use-package nerd-icons
  :ensure t)
(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))
(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

;;; ORG MODE
;; global binds to use org stuff in any mode
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; ignore scheduled tasks in the any view
(setq org-agenda-todo-ignore-scheduled 'future)

;; Custom capture templates - currently have some custom work templates configured
(setq org-capture-templates
      '(("i" "Inbox" entry (file+headline "~/sync/org/gtd.org" "Inbox")
	 "* INBOX %?\n %i\n %a")
        ("r" "Review request" entry (file+headline "~/sync/org/gtd.org" "Todos")
	 "* TODO review %?\nSCHEDULED: %t")
        ("t" "Today task" entry (file+headline "~/sync/org/gtd.org" "Todos")
	 "* TODO %?\nSCHEDULED: %t")
	))

;; allow refiling everywhere!
(setq org-refile-targets '((nil :maxlevel . 9)
			   (org-agenda-files :maxlevel . 9)))
(setq org-outline-path-complete-in-steps nil) ; Refile in a single go
(setq org-refile-use-outline-path t) ; Use full paths for refiling

;; WIP - custom weekly review view
;; from https://gettingthingsdone.com/wp-content/uploads/2014/10/Weekly_Review_Checklist.pdf
(setq org-agenda-custom-commands
      '(
	("1" "Execution"
	 ((agenda "" ((org-agenda-span 7)
		      (org-agenda-overriding-header "This week...")
		      (org-agenda-skip-function
		       '(org-agenda-skip-entry-if 'todo '("WAIT")))))
	  (todo "IN_PROGRESS"
		((org-agenda-overriding-header "WIP")))
	  (todo "WAITING"
		((org-agenda-overriding-header "Waiting")))
	  (todo "TODO"
		((org-agenda-overriding-header "Next actions")
		 (org-agenda-skip-function
		  '(org-agenda-skip-entry-if 'scheduled))))))
	("2" "Weekly Review"
	 ;; note: this "-" pattern is used to show steps for things not actionable in emacs yet.
	 ((todo "-"
		((org-agenda-overriding-header "Brain dump open thoughts into apple notes.")))
	  (todo "-"
		((org-agenda-overriding-header "Process inboxes to zero:\n  -Fastmail\n  -Gmail\n  -Apple Note\n  -Physical Inbox")))
	  (todo "TODO"
		((org-agenda-overriding-header "Review open TODOs; make sure all are still relevant.")))
	  (todo "-"
		((org-agenda-overriding-header "Review calendar data https://app.fastmail.com/calendar/month")))
          (todo "WAITING"
		((org-agenda-overriding-header "Review WAITING list.")))
          (tags "+projects+LEVEL=2"
	   	((org-agenda-overriding-header "Review projects; make sure each have a next action")))
	  (todo "-"
		;; Review Someday / maybe lists
		;; () TODO, implement this
		((org-agenda-overriding-header "Review Someday / Maybe")))))
	))

;; denote
(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n r" . denote-rename-file)
   ("C-c n l" . denote-link)
   ("C-c n b" . denote-backlinks)
   ("C-c n d" . denote-dired)
   ("C-c n g" . denote-grep))
  :config
  (setq denote-directory (expand-file-name "~/docs/notes"))

  ;; Automatically rename Denote buffers when opening them so that
  ;; instead of their long file name they have, for example, a literal
  ;; "[D]" followed by the file's title.  Read the doc string of
  ;; `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1))

;; EXPERIMENTAL
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(cmake "https://github.com/uyha/tree-sitter-cmake")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(go "https://github.com/tree-sitter/tree-sitter-go")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(make "https://github.com/alemuller/tree-sitter-make")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ("\\.Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.prisma\\'" . prisma-ts-mode)
         ;; More modes defined here...
         )
  :preface
  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (c-mode . c-ts-mode)
             (c++-mode . c++-ts-mode)
             (c-or-c++-mode . c-or-c++-ts-mode)
             (bash-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  )

(use-package magit
  :ensure t)

;; prevent the warning buffer from popping up always...
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
	       (display-buffer-no-window)
	       (allow-no-window . t)))

;;; eglot
;; Minimal Eglot configuration for TypeScript testing

;; Eglot is built-in for Emacs 29+
(require 'eglot)

;; Auto-start eglot for your tree-sitter TypeScript modes
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)
(add-hook 'js-ts-mode-hook 'eglot-ensure) ; for JavaScript files too

;; Basic eglot settings
(setq eglot-autoshutdown t)  ; Shutdown server when last buffer is killed
(setq eglot-sync-connect 1)  ; Wait 1 second for server connection

;; Eglot keybindings (using C-c e prefix to avoid your org C-c l binding)
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c e r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c e f") 'eglot-format-buffer)
  (define-key eglot-mode-map (kbd "C-c e a") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c e o") 'eglot-code-action-organize-imports)
  (define-key eglot-mode-map (kbd "C-c e h") 'eldoc-doc-buffer))

;; Note: Eglot will automatically find typescript-language-server
;; Make sure you have it installed: npm install -g typescript-language-server typescript

;;;; Code Completion
(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)           ; Minimum length of prefix for completion
  (corfu-auto-delay 0)            ; No delay for completion
  (corfu-popupinfo-delay '(0.5 . 0.2))  ; Automatically update info popup after that numver of seconds
  (corfu-preview-current 'insert) ; insert previewed candidate
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
	      ("M-SPC"      . corfu-insert-separator)
	      ("TAB"        . corfu-next)
	      ([tab]        . corfu-next)
	      ("S-TAB"      . corfu-previous)
	      ([backtab]    . corfu-previous)
	      ("S-<return>" . corfu-insert)
	      ("RET"        . corfu-insert))

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode) ; Popup completion info
  :config
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                                   corfu-quit-no-match t
                                   corfu-auto nil)
	      (corfu-mode))
            nil
            t))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
	      ("M-n" . flycheck-next-error) ; optional but recommended error navigation
	      ("M-p" . flycheck-previous-error)))

(use-package vterm
  :ensure t)

;;; TYPESCRIPT
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))


;; WANT TO DO EVENTUALLY
;; app shortcut for emacs.app

;;; APHELEIA
;; auto-format different source code files
(use-package apheleia
  :ensure t
  :config  (setq apheleia-formatters-respect-indent-level nil)
  ;; Add pnpm prettier formatter
  ;; (add-to-list 'apheleia-formatters
  ;;              '(prettier-pnpm . ("pnpm" "exec" "prettier" "--stdin-filepath" filepath)))
  
  ;; Associate it with TypeScript and other modes
  ;; (add-to-list 'apheleia-mode-alist '(typescript-ts-mode . prettier-pnpm))
  ;; (add-to-list 'apheleia-mode-alist '(js-mode . prettier-pnpm))
  ;; (add-to-list 'apheleia-mode-alist '(json-mode . prettier-pnpm))
  ;; (add-to-list 'apheleia-mode-alist '(css-mode . prettier-pnpm))
  
  ;; Enable globally
  (apheleia-global-mode +1))

;; set path from shell https://www.emacswiki.org/emacs/ExecPath
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$" "" (shell-command-to-string
					  "$SHELL --login -c 'echo $PATH'"
					  ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-PATH)


;;; LOAD WORK CONFIG
;; Load any work specific configurations that shouldn't live in github
(let ((work-config (expand-file-name "work-config.el" user-emacs-directory)))
  (when (file-exists-p work-config)
    (load work-config)))
