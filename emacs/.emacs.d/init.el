;;; init.el --- main config entry point
;;; Commentary:
;;; Code:

;; Delay garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Reset gc threshold value to default after startup
(add-hook 'after-init-hook (lambda ()
                             (setq gc-cons-threshold 800000)))

;;;; Paths
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(dolist (dir load-path)
  (make-directory dir t))

;; Packages inside XDG_DATA_HOME/emacs
(defvar user-data-directory
      (if (getenv "XDG_DATA_HOME")
          (getenv "XDG_DATA_HOME") "~/.local/share"))
(setq package-user-dir (expand-file-name "emacs" user-data-directory))
;; Cache inside XDG_CACHE_HOME/emacs
(defvar user-cache-directory
      (if (getenv "XDG_CACHE_HOME")
          (getenv "XDG_CACHE_HOME") "~/.cache"))

(defvar backup-dir   (expand-file-name "emacs/backup" user-cache-directory))
(defvar autosave-dir (expand-file-name "emacs/save" user-cache-directory))
(defvar undo-dir     (expand-file-name "emacs/undo" user-cache-directory))

(make-directory backup-dir t)
(make-directory autosave-dir t)
(make-directory undo-dir t)

;;;; Package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t
      use-package-always-ensure t
      bind-key-describe-special-forms t)

(use-package auto-compile
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

;;;; Settings
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-buffer-menu t
      initial-scratch-message nil
      echo-keystrokes 0.1
      create-lockfiles nil
      vc-follow-symlinks t)

;;; Backup
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t))
      auto-save-list-file-prefix autosave-dir
      backup-directory-alist `((".*" . ,backup-dir))
      version-control t
      kept-new-versions 2
      delete-old-versions t
      backup-by-copying-when-linked t
      vc-make-backup-files t)

;;;; Appearance
(when (fboundp #'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp #'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp #'scroll-bar-mode) (scroll-bar-mode -1))

(load-theme 'leuven)
(set-face-attribute 'default nil :family "monospace" :height 120)

;;;; Usability
(defalias #'yes-or-no-p #'y-or-n-p)

;; Undo and redo the window configuration
(winner-mode)

;; Window behaviour
(setq window-combination-resize t
      switch-to-buffer-preserve-window-point t)

;; Remember point position in files
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name "emacs/places" user-cache-directory)
      save-place-forget-unreadable-files nil)

;; Save mini buffer history
(require 'savehist)
(setq savehist-file (expand-file-name "emacs/history" user-cache-directory)
      history-length t
      history-delete-duplicates t)
(savehist-mode)

;; Bookmarks
(require 'bookmark)
(setq bookmark-default-file (expand-file-name "emacs/bookmarks" user-cache-directory))

;; Indicate buffer boundaries and empty lines
(setq-default indicate-buffer-boundaries 'left
              indicate-empty-lines t)

;; Word Wrap
(setq visual-line-fringe-indicators '(nil vertical-bar))
(diminish 'visual-line-mode "WP")
(global-visual-line-mode)

;; Highlight current line
(global-hl-line-mode)

;; Highlight matching parenthesis
(show-paren-mode)

;; Show argument list of the function call at cursor
(eldoc-mode)

;; Show column-number in the mode line
(column-number-mode)

;;;; Edit

;; Newline at end of file
(setq require-final-newline t)

;; Auto-insert matching delimiters
(electric-pair-mode)

;; Paragraph justification
(setq-default fill-column 72)
(setq auto-fill-mode t)

;;; Indentation
(setq-default tab-width 4
              standard-indent 4
              indent-tabs-mode nil)

(defun my-tab-width ()
  "Cycle 'tab-width' between values 2, 4, and 8."
  (interactive)
  (setq tab-width
        (cond ((eq tab-width 8) 2)
              ((eq tab-width 2) 4)
              (t 8)))
  (redraw-display))

(global-set-key (kbd "C-c t t") 'my-tab-width)

;;; White-space
(require 'whitespace)
(diminish 'whitespace-mode "WS")
(setq whitespace-line-column 100)
(add-hook 'before-save-hook 'whitespace-cleanup)

;;; Spell
(use-package flyspell
  :diminish (flyspell-mode . "FS")
  :bind ("C-c t s" . flyspell-mode)
  :commands (flyspell-mode flyspell-prog-mode)
  :init
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

;;;; Shell

;; Pager that works inside Emacs
(setenv "PAGER" "/usr/bin/cat")

;;;; Features

;;; Diff
(require 'ediff)
(setq ediff-window-setup-function #'ediff-setup-windows-plain
      ediff-split-window-function #'split-window-horizontally)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;;; Undo
(use-package undo-tree
  :diminish (undo-tree-mode . "UT")
  :commands global-undo-tree-mode
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-history-directory-alist `((".*" . ,undo-dir))
        undo-tree-auto-save-history t
        undo-tree-visualizer-diff t
        undo-tree-visualizer-timestamps t))

;;; Completion
(setq completion-cycle-threshold 5
      tab-always-indent 'complete)

(defun indent-or-complete ()
  (interactive)
  (if (looking-at "\\_>")
    (company-complete-common)
    (indent-according-to-mode)))

(use-package company
  :diminish (company-mode . "CY")
  :bind (:map company-mode-map
         ("<tab>" . indent-or-complete)
         :map company-active-map
         ("<tab>" . company-complete-common-or-cycle)
         ("<backtab>" . company-select-previous)
         ("<escape>" . company-abort)
         ("<return>" . nil))
  :commands global-company-mode
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  :config
  (setq company-minimum-prefix-length 2
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-show-numbers t
        company-require-match 'never
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case t
        company-selection-wrap-around t))

(use-package company-statistics
  :commands company-statistics-mode
  :init
  (add-hook 'after-init-hook #'company-statistics-mode)
  :config
  (setq company-statistics-file (expand-file-name "emacs/company-statistics-cache.Eli" user-cache-directory)))

;;; VIM
(use-package evil
  :bind ("C-c t v" . evil-mode))

;;; Helm
(use-package helm
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-mini)
         ("M-y" . helm-show-kill-ring))
  :commands (helm-mode helm-autoresize-mode)
  :init
  (add-hook 'after-init-hook #'helm-mode)
  (add-hook 'after-init-hook #'helm-autoresize-mode)
  :config
  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-display-header-line nil))

(use-package helm-projectile
  :commands help-projectile-on
  :init
  (add-hook 'after-init-hook #'helm-projectile-on)
  :config
  (setq projectile-completion-system 'helm)
  (setq helm-projectile-fuzzy-match t))

(use-package helm-ag)

;;; Linting
(use-package flycheck
  :commands flycheck-mode
  :init
  (add-hook 'prog-mode-hook #'flycheck-mode)
  :config
  (setq-default flycheck-emacs-lisp-load-path 'inherit))

;;; File navigation
(use-package projectile
  :diminish projectile-mode
  :commands projectile-global-mode
  :init
  (add-hook 'after-init-hook #'projectile-global-mode)
  :config
  (setq projectile-enable-caching t
        projectile-cache-file (expand-file-name "emacs/projectile.cache" user-cache-directory)
        projectile-known-projects-file (expand-file-name "emacs/projectile-bookmarks.eld" user-cache-directory)))

;;; Zoom window
(use-package zoom-window
  :bind ("C-c C-z" . zoom-window-zoom)
  :config
  (setq zoom-window-mode-line-color "DarkGreen"))

;;;; Keys
(define-key emacs-lisp-mode-map
  (kbd "M-.") 'find-function-at-point)

;; Increase/decrease font-size with scroll
(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)

(bind-key [remap dabbrev-expand] #'hippie-expand)

(bind-key "C-c t l" #'linum-mode)
(bind-key "C-c t r" #'ruler-mode)
(bind-key "C-c t w" #'whitespace-mode)

(bind-key "C-c f e" #'ediff)
(bind-key "C-c f w" #'whitespace-cleanup)
;;; init.el ends here
