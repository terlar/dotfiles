;;; init.el --- main config entry point
;;; Commentary:
;;; Code:

;; Delay garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Reset gc threshold value to default after startup
(add-hook 'after-init-hook (lambda ()
                             (setq gc-cons-threshold 800000)))

;;; Paths
(eval-and-compile
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
  (dolist (dir load-path)
    (make-directory dir t)))

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

;;; Package base
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(require 'diminish)
(setq use-package-verbose t
      use-package-always-ensure t
      bind-key-describe-special-forms t)

;;; Settings
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-buffer-menu t
      initial-scratch-message nil
      echo-keystrokes 0.1
      create-lockfiles nil
      vc-follow-symlinks t
      load-prefer-newer t
      completion-cycle-threshold 5
      tab-always-indent 'complete)

;; Backup
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t))
      auto-save-list-file-prefix autosave-dir
      backup-directory-alist `((".*" . ,backup-dir))
      version-control t
      kept-new-versions 2
      delete-old-versions t
      backup-by-copying-when-linked t
      vc-make-backup-files t)

;;; Appearance
(when (fboundp #'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp #'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp #'scroll-bar-mode) (scroll-bar-mode -1))

(load-theme 'leuven)
(set-face-attribute 'default nil :family "monospace" :height 120)

(defun on-frame-open (&optional frame)
  "If the FRAME created in terminal don't load background color."
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)))

(add-hook 'after-make-frame-functions 'on-frame-open)

;;; Libraries
(use-package auto-compile
  :config
  (auto-compile-on-load-mode))

;;; Enable disabled commands
(put 'downcase-region 'disabled nil) ; Let downcasing work

;;; Keys
(defalias #'yes-or-no-p #'y-or-n-p)

;; Increase/decrease font-size with scroll
(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)

(bind-key [remap dabbrev-expand] #'hippie-expand)

;; C-
(defvar ctl-period-map)
(define-prefix-command 'ctl-period-map)
(bind-key "C-." #'ctl-period-map)

(bind-key* "<C-return>" #'other-window)

;; M-
(bind-key "M-W" #'mark-word)

(defun mark-line (&optional arg)
  (interactive "p")
  (beginning-of-line)
  (let ((here (point)))
    (dotimes (i arg)
      (end-of-line))
    (set-mark (point))
    (goto-char here)))

(bind-key "M-L" #'mark-line)

(defun mark-sentence (&optional arg)
  (interactive "P")
  (backward-sentence)
  (mark-end-of-sentence arg))

(bind-key "M-S" #'mark-sentence)
(bind-key "M-X" #'mark-sexp)
(bind-key "M-D" #'mark-defun)

(bind-key "M-g c" #'goto-char)
(bind-key "M-g l" #'goto-line)

(define-key emacs-lisp-mode-map
  (kbd "M-.") 'find-function-at-point)

;; C-c
(bind-key "C-c <tab>" #'ff-find-other-file)

(defun delete-current-line (&optional arg)
  (interactive "p")
  (let ((here (point)))
    (beginning-of-line)
    (kill-line arg)
    (goto-char here)))

(bind-key "C-c d" #'delete-current-line)
(bind-key "C-c q" #'fill-region)
(bind-key "C-c r" #'replace-regexp)
(bind-key "C-c s" #'replace-string)
(bind-key "C-c u" #'rename-uniquely)

(bind-key "C-c ;" #'comment-or-uncomment-region)

;; C-c t (Toggle)
(bind-keys :prefix-map toggle-map
           :prefix "C-c t"
           ("d" . toggle-debug-on-error)
           ("l" . linum-mode)
           ("r" . ruler-mode)
           ("w" . whitespace-mode))

(bind-key "C-c f e" #'ediff)
(bind-key "C-c f u" #'undo-tree-visualize)

;;; Usability
(semantic-mode)

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

;; Indicate buffer boundaries and empty lines
(setq-default indicate-buffer-boundaries 'left
              indicate-empty-lines t)

;; Word Wrap
(diminish 'visual-line-mode "WP")
(global-visual-line-mode)

;; Fringe indicators
(setq visual-line-fringe-indicators '(nil vertical-bar))

;; Highlight current line
(global-hl-line-mode)

;; Highlight matching parenthesis
(show-paren-mode)

;; Show column-number in the mode line
(column-number-mode)

;;; Edit

;; Newline at end of file
(setq require-final-newline t)

;; Paragraph justification
(setq-default fill-column 72)
(setq auto-fill-mode t)

;; Indentation
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

;;; Shell

;; Pager that works inside Emacs
(setenv "PAGER" "/usr/bin/cat")

;;; Packages
(use-package ag ;; File grep/search
  :commands (ag ag-regexp)
  :init
  (use-package helm-ag
    :commands helm-ag))

(use-package align
  :commands align
  :bind (("M-[" . align-code)
         ("C-c [" . align-regexp))
  :preface
  (defun align-code (beg end &optional arg)
    (interactive "rP")
    (if (null arg)
        (align beg end)
      (let ((end-mark (copy-marker end)))
        (indent-region beg end-mark nil)
        (align beg end-mark)))))

(use-package autorevert
  :diminish auto-revert-mode
  :commands auto-revert-mode
  :init
  (add-hook 'find-file-hook #'(lambda () (auto-revert-mode 1))))

(use-package bookmark
  :defer 10
  :config
  (setq bookmark-default-file (expand-file-name "emacs/bookmarks" user-cache-directory)))

(use-package company ;; Completion
  :diminish company-mode
  :commands global-company-mode
  :defer 10
  :bind (:map company-mode-map
         ("<tab>" . indent-or-complete)
         :map company-active-map
         ("<tab>" . company-complete-common-or-cycle)
         ("<backtab>" . company-select-previous)
         ("<escape>" . company-abort)
         ("<return>" . nil))
  :preface
  (defun indent-or-complete ()
    "Try to indent before trying to complete."
    (interactive)
    (if (looking-at "\\_>")
        (company-complete-common)
      (indent-according-to-mode)))
  :config
  (use-package company-statistics
    :config
    (setq company-statistics-file (expand-file-name "emacs/company-statistics-cache.el" user-cache-directory))
    (company-statistics-mode))
  (setq company-minimum-prefix-length 2
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-show-numbers t
        company-require-match 'never
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case t
        company-selection-wrap-around t)
  (global-company-mode))

(use-package compile
  :bind (("C-c c" . compile)))

(use-package dired+
  :config
  (setq diredp-toggle-find-file-reuse-dir 1
        diredp-image-preview-in-tooltip 300))

(use-package dired-toggle
  :config
  :bind ("C-. d" . dired-toggle))

(use-package dtrt-indent ;; Auto-detect indentation mode
  :defer t
  :diminish dtrt-indent-mode
  :commands dtrt-indent-mode
  :init
  (add-hook 'prog-mode-hook #'dtrt-indent-mode))

(use-package ediff
  :requires winner
  :bind (("C-. = b" . ediff-buffers)
         ("C-. = B" . ediff-buffers3)
         ("C-. = c" . compare-windows)
         ("C-. = =" . ediff-files)
         ("C-. = f" . ediff-files)
         ("C-. = F" . ediff-files3)
         ("C-. = r" . ediff-revision)
         ("C-. = p" . ediff-patch-file)
         ("C-. = P" . ediff-patch-buffer)
         ("C-. = l" . ediff-regions-linewise)
         ("C-. = w" . ediff-regions-wordwise))
  :init
  (defvar ctl-period-equals-map)
  (define-prefix-command 'ctl-period-equals-map)
  (bind-key "C-. =" #'ctl-period-equals-map)
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo)
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally))

(use-package eldoc ;; Show argument list/type information in the modeline
  :diminish eldoc-mode
  :config
  (eldoc-mode))

(use-package electric
  :config
  ;; Auto-insert matching delimiters
  (electric-pair-mode)
  (electric-indent-mode))

(use-package etags
  :bind ("M-T" . tags-search))

(use-package evil ;; VIM-like
  :bind ("C-c t v" . evil-mode)
  :config
  (use-package evil-leader
    :config
    (evil-leader/set-leader "<SPC>")
    (global-evil-leader-mode))
  (use-package evil-surround
    :config
    (global-evil-surround-mode))
  ;; Insert state uses Emacs key-map.
  (setq evil-insert-state-map (make-sparse-keymap))
  (define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state))

(use-package flycheck ;; Linting
  :commands flycheck-mode
  :defer 5
  :config
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (global-flycheck-mode))

(use-package flyspell
  :diminish flyspell-mode
  :commands (flyspell-mode flyspell-prog-mode)
  :bind ("C-c t s" . flyspell-mode)
  :init
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  :config
  (unbind-key "C-." flyspell-mode-map))

(use-package git-messenger
  :bind ("C-x v m" . git-messenger:popup-message))

(use-package helm-swoop
  :bind (("M-s o" . helm-swoop)
         ("M-s /" . helm-multi-swoop)))

(use-package helm-descbinds
  :bind ("C-h b" . helm-descbinds)
  :init
  (fset 'describe-bindings 'helm-descbinds)
  :config
  (require 'helm))

(use-package helm ;; Completion system
  :demand t
  :diminish helm-mode
  :bind (("C-h a"   . helm-apropos)
         ("C-x f"   . helm-multi-files)
         ("C-x C-f" . helm-find-files)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b"   . helm-mini)
         ("M-x"     . helm-M-x)
         ("M-y"     . helm-show-kill-ring)
         ("M-s b"   . helm-occur)
         ("M-H"     . helm-resume))
  :bind-keymap ("C-c h" . helm-command-prefix)
  :config
  (use-package helm-helm-commands)

  (helm-mode)
  (helm-autoresize-mode)

  (bind-key "<tab>" #'helm-execute-persistent-action helm-map)
  (bind-key "C-i" #'helm-execute-persistent-action helm-map)
  (bind-key "C-z" #'helm-select-action helm-map)
  (bind-key "A-v" #'helm-previous-page helm-map)

  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-display-header-line nil))

(use-package magit
  :bind (("C-x g" . magit-status))
  :preface
  (defun magit-monitor (&optional no-display)
    "Start git-monitor in the current directory."
    (interactive)
    (when (string-match "\\*magit: \\(.+\\)" (buffer-name))
      (let ((name (format "*git-monitor: %s*"
                          (match-string 1 (buffer-name)))))
        (or (get-buffer name)
            (let ((buf (get-buffer-create name)))
              (ignore-errors
                (start-process "*git-monitor*" buf "git-monitor"
                               "-d" (expand-file-name default-directory)))
              buf)))))
  :config
  (setenv "GIT_PAGER" "")
  (use-package evil-magit)

  (unbind-key "<C-return>" magit-file-section-map)

  (evil-leader/set-key "g" 'magit-status)

  (add-hook 'magit-status-mode-hook #'(lambda () (magit-monitor t))))

(use-package projectile
  :diminish projectile-mode
  :commands projectile-global-mode
  :defer 5
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (use-package helm-projectile
    :config
    (setq projectile-completion-system 'helm
          helm-projectile-fuzzy-match t)
    (helm-projectile-on))
  (setq projectile-enable-caching t
        projectile-cache-file (expand-file-name "emacs/projectile.cache" user-cache-directory)
        projectile-known-projects-file (expand-file-name "emacs/projectile-bookmarks.eld" user-cache-directory))
  (projectile-global-mode))

(use-package undo-tree
  :diminish (undo-tree-mode . "UT")
  :commands global-undo-tree-mode
  :config
  (setq undo-tree-history-directory-alist `((".*" . ,undo-dir))
        undo-tree-auto-save-history t
        undo-tree-visualizer-diff t
        undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode))

(use-package whitespace
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode)
  :commands (whitespace-buffer
             whitespace-cleanup
             whitespace-mode)
  :defines (whitespace-auto-cleanup
            whitespace-rescan-timer-time
            whitespace-silent)
  :init
  (add-hook 'before-save-hook 'whitespace-cleanup)
  :config
  (remove-hook 'find-file-hooks 'whitespace-buffer)
  (remove-hook 'kill-buffer-hook 'whitespace-buffer)
  (setq whitespace-auto-cleanup t
        whitespace-line-column 100
        whitespace-silent t))

(use-package winner
  :if (not noninteractive)
  :defer 5
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :config
  (winner-mode))

(use-package zoom-window ;; Temporary one window
  :bind ("C-c C-z" . zoom-window-zoom)
  :config
  (setq zoom-window-mode-line-color "DarkGreen"))

;;; Language packages
(use-package css-mode
  :mode "\\.css\\'")

(use-package dockerfile-mode
  :mode "\\Dockerfile\\'")

(use-package elixir-mode
  :mode ("\\.ex\\'" "\\.exs\\'" "mix\\.lock\\'")
  :config
  (use-package alchemist
    :diminish alchemist-mode
    :config
    (exec-path-from-shell-copy-env "MIX_ARCHIVES")
    (setq alchemist-test-status-modeline nil)))

(use-package erlang
  :mode ("\\.erl\\'" . erlang-mode))

(use-package go-mode
  :mode "\\.go\\'"
  :preface
  (defun my-go-mode-hook ()
    ;; Use goimports
    (setq gofmt-command "goimports")
    ;; Run gofmt before save
    (add-hook 'before-save-hook 'gofmt-before-save)
    ;; Customize compile command to run go build
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go build -v && go test -v && go vet"))
    ;; godef jump key binding
    (local-set-key (kbd "M-.") 'godef-jump))
  :config
  (use-package go-eldoc)
  (use-package company-go)

  (add-hook 'go-mode-hook #'go-eldoc-setup)
  (add-hook 'go-mode-hook #'my-go-mode-hook))

(use-package haskell-mode
  :mode "\\.l?hs\\'"
  :config
  (use-package flycheck-haskell
    :config
    (flycheck-haskell-setup)
    (bind-key "M-n" #'flycheck-next-error haskell-mode-map)
    (bind-key "M-p" #'flycheck-previous-error haskell-mode-map)))

(use-package js2-mode
  :mode "\\.js\\'")

(use-package json-mode
  :mode "\\.json\\'")

(use-package markdown-mode
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode)))

(use-package puppet-mode
  :mode ("\\.pp\\'" . puppet-mode))

(use-package python-mode
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

(use-package ruby-mode
  :mode ("\\.rb\\'" . ruby-mode)
  :interpreter ("ruby" . ruby-mode)
  :functions inf-ruby-keys
  :config
  (use-package yari))

(use-package rust-mode
  :mode ("\\.rust\\'" . rust-mode)
  :config
  (use-package flycheck-rust
    :init
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode))
;;; init.el ends here
