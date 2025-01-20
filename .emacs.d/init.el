;; parker's emacs configuration

;; On macOS, I typically run `emacs-plus` with some options
;; brew install emacs-plus --with-no-frame-refocus --with-native-comp --with-savchenkovaleriy-big-sur-3d-icon
;; --with-no-frame-refocus doesn't focus the next frame once emacs is closed
;; --with-native-comp for better perf
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
(tool-bar-mode -1) ;; Disable toolbar at the top of the screen
(scroll-bar-mode -1) ;; Disable scroll bars (might turn these back on)
(menu-bar-mode 1) ;; Keep the menu bar enabled on MacOS (might disable this in other envs)
(global-display-line-numbers-mode 1) ;; enable line numbers
(setq display-line-numbers-type 'visual) ;; don't account for folded code, this is nice for vim motions
(use-package nord-theme
  :ensure t
  :config
  (load-theme 'nord :no-confirm-loading))

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
;; The parent *Projects heading has a :project: tag. All level two headings under this are to
;; be evaluated as stuck projects
(setq org-stuck-projects
'("+projects+LEVEL=2"
 ("TODO" "NEXT" "NEXTACTION")
 nil ""))

;; WIP - custom weekly review view
;; from https://gettingthingsdone.com/wp-content/uploads/2014/10/Weekly_Review_Checklist.pdf
(setq org-agenda-custom-commands
      '(("W" "Weekly Review"
         (
	  ;; brain dump
	  ;; () TODO - brain dump into inbox? agenda view? :thinking:

	  ;; Clear inboxes
	  ;; () TODO - can I have that here?

	  ;; Review open actions, make sure they are still relevant
	  (todo "TODO"
		((org-agenda-overriding-header "Review open actions")))

	  ;; review prev and upcoming calendar data
	  ;; () TODO add all calendar data to org mode
	    ;; some command like this... (agenda "" ((org-agenda-span 7)))

	  ;; Review waiting for
          (todo "WAITING"
		((org-agenda-overriding-header "Review 'Waiting For'")))

	  ;; Review all active projects, make sure they're actionable and relevant
          (stuck ""
		((org-agenda-overriding-header "Add next actions to stuck projects")))
          (tags "+projects+LEVEL=2"
		((org-agenda-overriding-header "Review Projects")))

	  )

	  ;; Review Someday / maybe lists
	  ;; () TODO, implement this
	 )
	("E" "Execution" 
	 (
	  (agenda "" ((org-agenda-span 7))))
	 ))
	 )
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Sync/org-roam")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))
