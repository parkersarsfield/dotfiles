;; parker's emacs configuration

;; On macOS, I typically run `emacs-plus` with some options
;; brew install emacs-plus@30 --with-savchenkovaleriy-big-sur-3d-icon
;; --with-savchenkovaleriy-big-sur-3d-icon for pretty icon

;;; GENERAL / STARTUP
;; setup MELPA
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

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


;; consider setting up corfu


;;; APPEARANCE
(setq frame-resize-pixelwise t) ; get rid of annoying gaps
(tool-bar-mode -1) ;; Disable toolbar at the top of the screen
(scroll-bar-mode -1) ;; Disable scroll bars (might turn these back on)
(menu-bar-mode 1) ;; Keep the menu bar enabled on MacOS (might disable this in other envs)
(global-display-line-numbers-mode 1) ;; enable line numbers
(setq display-line-numbers-type 'visual) ;; don't account for folded code, this is nce for vim motions
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)) ;; transparent titlebar
(add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; dark mode
(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi-tinted :no-confirm-loading))
;; dissabled while testing out modus themes
;; (use-package nord-theme
;;   :ensure t
;;   :config
;;   (load-theme 'nord :no-confirm-loading))

;; use preferred fonts
(let ((mono-spaced-font "Monospace")
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
      '(("i" "Inbox" entry (file+headline "~/path/to/gtd.org" "Inbox")
	 "* INBOX %?\n %i\n %a")
        ("r" "Review request" entry (file+headline "~/path/to/gtd.org" "Todos")
	 "* TODO review %?\nSCHEDULED: %t")
        ("t" "Today task" entry (file+headline "~/path/to/gtd.org" "Todos")
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
      '(("W" "Weekly Review"
         (
	  ;; this "-" pattern is used to show steps for things not actionable in emacs yet.
	  (todo "-"
		((org-agenda-overriding-header "Brain dump open thoughts into apple notes.")))
	  (todo "-"
		((org-agenda-overriding-header "Process inboxes to zero:\n  -Fastmail\n  -Gmail\n  -Apple Note\n  -Physical Inbox")))
	  (todo "TODO"
		((org-agenda-overriding-header "Review open TODOs; make sure all are still relevant.")))
	  (todo "-"
		;; () TODO add all calendar data to org mode
		;; some command like this... (agenda "" ((org-agenda-span 7)))
		((org-agenda-overriding-header "Review calendar data https://app.fastmail.com/calendar/month")))
          (todo "WAITING"
		((org-agenda-overriding-header "Review WAITING list.")))
           (tags "+projects+LEVEL=2"
	   	((org-agenda-overriding-header "Review projects; make sure each have a next action")))
	  (todo "-"
		;; Review Someday / maybe lists
		;; () TODO, implement this
		((org-agenda-overriding-header "Review Someday / Maybe")))
	  )
	 )
	("E" "Execution" 
	 (
	  (agenda "" ((org-agenda-span 7) (org-agenda-overriding-header "This week...")))
	  (todo "TODO"
		((org-agenda-overriding-header "Next actions")))
	 )
	 ))
	 )

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
