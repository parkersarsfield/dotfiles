;; parker's emacs configuration

;; On macOS, I typically run `emacs-plus` with some options
;; brew install emacs-plus --with-no-frame-refocus --with-native-comp --with-savchenkovaleriy-big-sur-3d-icon
;; --with-no-frame-refocus doesn't focus the next frame once emacs is closed
;; --with-native-comp for better perf
;; --with-savchenkovaleriy-big-sur-3d-icon for pretty icon

;; Include MELPA in package registry
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

;; Don't show the splash screen upon startup, maybe I should replace this with something nicer...
(setq inhibit-startup-message t)

;;; KEYBINDINGS
;; Enable Evil Mode
(unless (package-installed-p 'evil)
  (package-install 'evil))
(require 'evil)
(evil-mode 1)

;;; APPEARANCE
(tool-bar-mode -1) ;; Disable toolbar at the top of the screen (I've never jsed this...)
(scroll-bar-mode -1) ;; Disable scroll bars (might turn these back on)
(load-theme 'nord t) ;; enable nord theme
(global-display-line-numbers-mode 1) ;; enable line numbers
(setq display-line-numbers-type 'visual) ;; don't account for folded code, this is nice for vim motions
