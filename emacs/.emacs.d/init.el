;;; init.el --- main configuration entry point

;;; Commentary:

;; Emacs configuration of Terje Larsen.

;;; Code:
;;; -*- lexical-binding: t -*-

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
(require 'use-package)
(setq use-package-always-ensure t
      bind-key-describe-special-forms t)

;;; Settings
(prefer-coding-system 'utf-8)

(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-buffer-menu t
      ad-redefinition-action 'accept
      initial-scratch-message nil
      echo-keystrokes 0.1
      completion-cycle-threshold 5
      tab-always-indent 'complete
      uniquify-buffer-name-style 'forward)

;; Files
(setq create-lockfiles nil
      load-prefer-newer t
      vc-follow-symlinks t)

;; Kill-ring
(setq kill-ring-max 200
      kill-do-not-save-duplicates t
      save-interprogram-paste-before-kill t)

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

(use-package twilight-bright-theme
  :config
  (load-theme 'twilight-bright t))

(set-face-attribute 'default nil
                    :family "Input Mono" :height 120)
(set-face-attribute 'variable-pitch nil
                    :family "Merriweather Sans" :height 120)
(copy-face 'default 'fixed-pitch)

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

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Increase/decrease font-size with scroll
(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)

;; C-
(defun kill-region-or-backward-kill-word (&optional arg region)
  "Takes ARG and REGION and passes to:
`kill-region' if the region is active, otherwise `backward-kill-word'."
  (interactive
   (list (prefix-numeric-value current-prefix-arg) (use-region-p)))
  (if region
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

(bind-key "C-w" #'kill-region-or-backward-kill-word)

;; M-
(bind-key "M-W" #'mark-word)

(defun mark-line ()
  "Mark the current line."
  (interactive)
  (beginning-of-line)
  (let ((here (point)))
    (end-of-line)
    (set-mark (point))
    (goto-char here)))

(bind-key "M-L" #'mark-line)

(defun mark-sentence ()
  "Mark the current sentence."
  (interactive)
  (backward-sentence)
  (mark-end-of-sentence 1))

(bind-key "M-S" #'mark-sentence)
(bind-key "M-X" #'mark-sexp)
(bind-key "M-D" #'mark-defun)

(bind-key "M-g c" #'goto-char)
(bind-key "M-g l" #'goto-line)

(define-key emacs-lisp-mode-map
  (kbd "M-.") 'find-function-at-point)

;; C-x
(defun my-tab-width ()
  "Cycle 'tab-width' between values 2, 4, and 8."
  (interactive)
  (setq tab-width
        (cond ((eq tab-width 8) 2)
              ((eq tab-width 2) 4)
              (t 8)))
  (redraw-display))

(bind-key "C-x t" #'my-tab-width)

;; C-c
(bind-key "C-c <tab>" #'ff-find-other-file) ; Open alternate file

(defun delete-current-line ()
  "Delete the current line."
  (interactive)
  (let ((here (point)))
    (beginning-of-line)
    (kill-line t)
    (goto-char here)))

(bind-key "C-c d" #'delete-current-line)
(bind-key "C-c q" #'fill-region)
(bind-key "C-c ;" #'comment-or-uncomment-region)

;; C-c t (Toggle)
(bind-keys :prefix-map toggle-map
           :prefix "C-c t"
           ("d" . toggle-debug-on-error)
           ("l" . linum-mode)
           ("r" . ruler-mode)
           ("v" . variable-pitch-mode)) ; Toggle fixed-width/variable-width

;; C-c w (Window)
(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(bind-keys :prefix-map window-map
           :prefix "C-c w"
           ("=" . balance-windows)
           ("k" . delete-window)
           ("/" . split-window-right)
           ("-" . split-window-below)
           ("m" . delete-other-windows)
           ("u" . rename-uniquely)
           ("b" . switch-to-minibuffer))

;;; Usability

;; Isearch
(setq isearch-allow-scroll t)
(diminish 'isearch-mode)

;; Window behavior
(setq window-combination-resize t
      switch-to-buffer-preserve-window-point t)

;; Fringe indicators
(setq-default indicate-buffer-boundaries 'left
              indicate-empty-lines t)
(setq visual-line-fringe-indicators '(left-curly-arrow
                                      right-curly-arrow))

;; Visual word wrapping
(diminish 'visual-line-mode)
(global-visual-line-mode)

;; Highlight matching parenthesis
(show-paren-mode)

;; Show line and column number in the mode line
(line-number-mode)
(column-number-mode)

;;; Editing

(setq-default line-spacing 0.2) ; Increase line spacing

;; Newline at end of file
(setq indicate-empty-lines t
      require-final-newline t)

;; Word wrapping
(setq-default fill-column 72)
(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'prog-mode-hook #'auto-fill-mode)
(diminish 'auto-fill-function " Ⓕ")

;; Indentation
(setq-default indent-tabs-mode t
              tab-width 4)

;;; Shell

;; Pager that works inside Emacs
(setenv "PAGER" "/usr/bin/cat")

;;; Builtin packages
(use-package align ; Align text
  :bind
  (("C-c [" . align-regexp)
   ("C-c x a a" . align)
   ("C-c x a c" . align-current))
  :commands align)

(use-package autorevert ; Auto-revert buffers of changed files
  :init
  (global-auto-revert-mode)
  :config
  (setq auto-revert-verbose nil
        global-auto-revert-non-file-buffers t)
  :diminish (auto-revert-mode . " Ⓐ"))

(use-package bookmark
  :bind
  ("C-c f b" . list-bookmarks)
  :config
  (setq bookmark-default-file (expand-file-name "emacs/bookmarks" user-cache-directory)
        bookmark-save-flag t))

(use-package compile
  :bind
  ("C-c c C" . recompile)
  :config
  (setq compilation-ask-about-save nil
        compilation-always-kill t
        ;; Automatically scroll
        compilation-scroll-output 'first-error
        ;; Skip warnings and info messages
        compilation-skip-threshold 2
        compilation-disable-input t
        compilation-context-lines 3)

  ;; Filter ANSI escape codes in compilation-mode output
  (require 'ansi-color)
  (add-hook 'compilation-filter-hook
            (lambda ()
              (let ((inhibit-read-only t))
                (ansi-color-apply-on-region compilation-filter-start
                                            (point)))))
  :commands ansi-color-apply-on-region)

(use-package delsel
  :defer t
  :init (delete-selection-mode))

(use-package ediff
  :bind
  (("C-c = b" . ediff-buffers)
   ("C-c = B" . ediff-buffers3)
   ("C-c = c" . compare-windows)
   ("C-c = =" . ediff-files)
   ("C-c = f" . ediff-files)
   ("C-c = F" . ediff-files3)
   ("C-c = r" . ediff-revision)
   ("C-c = p" . ediff-patch-file)
   ("C-c = P" . ediff-patch-buffer)
   ("C-c = l" . ediff-regions-linewise)
   ("C-c = w" . ediff-regions-wordwise))
  :init
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo)
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally)
  :commands ediff-setup-windows-plain)

(use-package eldoc ; Documentation in minibuffer
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  :diminish (eldoc-mode . " ⓓ"))

(use-package electric
  :defer t
  :config
  (electric-indent-mode)
  ;; Auto-insert matching delimiters
  (electric-pair-mode))

(use-package etags
  :bind
  ("M-T" . tags-search)
  :config
  (setq tags-revert-without-query t))

(use-package eww
  :bind
  (("C-c a w w" . eww)
   ("C-c a w u" . eww-browse-url))
  :init
  (add-hook 'eww-mode-hook #'buffer-face-mode))

(use-package flyspell
  :bind
  ("C-c t s" . flyspell-mode)
  :init
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'message-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  :config
  (setq flyspell-use-meta-tab nil
        flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil)
  (unbind-key "C-." flyspell-mode-map)
  :diminish (flyspell-mode . " ⓢ"))

(use-package hippie-exp ; Expansion and completion (line)
  :bind (([remap dabbrev-expand] . hippie-expand))
  :config
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

(use-package hl-line ; Highlight current line
  :defer t
  :init
  (global-hl-line-mode))

(use-package image-file
  :defer t
  :init
  (auto-image-file-mode))

(use-package outline ; Navigate outlines in buffers
  :defer t
  :init
  (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook #'outline-minor-mode))
  :diminish outline-minor-mode)

(use-package recentf
  :defer t
  :init
  (recentf-mode)
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        recentf-auto-cleanup 300
        recentf-exclude (list "/\\.git/.*\\'"  ; Git contents
                              "/elpa/.*\\'"))) ; Package files

(use-package savehist ; Save mini buffer history
  :defer t
  :init
  (savehist-mode)
  :config
  (setq savehist-file (expand-file-name "emacs/history" user-cache-directory)
        savehist-save-minibuffer-history t
        history-length t
        history-delete-duplicates t))

(use-package saveplace ; Remember point position in files
  :defer t
  :init
  (setq-default save-place t)
  :config
  (setq save-place-file (expand-file-name "emacs/places" user-cache-directory)
        save-place-forget-unreadable-files nil))

(use-package subword ; Recognize camel and snake case
  :defer t
  :init
  (add-hook 'prog-mode-hook #'subword-mode)
  :diminish subword-mode)

(use-package whitespace
  :bind
  ("C-c t w" . whitespace-mode)
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook #'whitespace-cleanup))
  :config
  (setq whitespace-line-column 100)
  :diminish (whitespace-mode . " ⓦ"))

(use-package windmove ; Move between windows
  :bind
  (("C-c w <left>"  . windmove-left)
   ("C-c w <right>" . windmove-right)
   ("C-c w <up>"    . windmove-up)
   ("C-c w <down>"  . windmove-down)))

(use-package winner ; Undo and redo window configuration
  :bind
  (("M-N" . winner-redo)
   ("M-P" . winner-undo))
  :init
  (winner-mode))

;;; Packages
(use-package ace-window ; Fast window switching
  :bind (("<C-return>" . ace-window)
         ("C-c w w"    . ace-window)))

(use-package adaptive-wrap ; Align wrapped lines
  :defer t
  :init (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

(use-package ag ; File grep/search
  :config
  (use-package helm-ag
    :bind
    (("C-c s a" . helm-ag)
     ("C-c s A" . helm-do-ag))
    :config
    (setq helm-ag-fuzzy-match t
          helm-ag-insert-at-point 'symbol
          helm-ag-edit-save t)
    :commands (helm-ag helm-do-ag))
  :commands (ag ag-regexp))

(use-package aggressive-indent
  :defer 5
  :init
  (global-aggressive-indent-mode t)
  :diminish aggressive-indent-mode)

(use-package anzu ; Position/matches count for search
  :bind
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)
   :map isearch-mode-map
   ([remap isearch-query-replace] . anzu-isearch-query-replace)
   ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :init
  (global-anzu-mode)
  :config
  (use-package evil-anzu)
  :commands
  (anzu--update
   anzu--reset-mode-line
   anzu--cons-mode-line-search)
  :diminish anzu-mode)

(use-package auto-dictionary ; Automatically infer dictionary
  :bind
  (("C-c l l" . adict-change-dictionary)
   ("C-c l g" . adict-guess-dictionary))
  :init
  (add-hook 'flyspell-mode-hook #'auto-dictionary-mode))

(use-package company ; Completion
  :bind
  (("<tab>" . indent-or-complete)
   :map company-active-map
   ("C-e"       . company-complete-selection)
   ("C-f"       . company-complete-selection)
   ("TAB"       . company-complete-common-or-cycle)
   ("<tab>"     . company-complete-common-or-cycle)
   ("S-TAB"     . company-select-previous)
   ("<backtab>" . company-select-previous)
   ("RET"       . nil)
   ("<return>"  . nil)
   ("<escape>"  . company-abort))
  :init
  (global-company-mode)
  (defun indent-or-complete ()
    "Try to indent before trying to complete."
    (interactive)
    (if (looking-at "\\_>")
        (company-complete-common-or-cycle)
      (indent-according-to-mode)))
  :config
  (use-package company-statistics
    :init
    (company-statistics-mode)
    :config
    (setq company-statistics-file (expand-file-name
                                   "emacs/company-statistics-cache.el" user-cache-directory)))
  (use-package company-quickhelp
    :init
    (company-quickhelp-mode))
  (setq company-minimum-prefix-length 2
        company-idle-delay 0
        company-show-numbers t
        company-require-match 'never
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-selection-wrap-around t)
  :diminish company-mode)

(use-package dired+
  :defer 1
  :init
  (setq diredp-image-preview-in-tooltip 300
        ;; Remove color and font decoration
        font-lock-maximum-decoration (quote ((dired-mode) (t . t))))
  :config
  (diredp-toggle-find-file-reuse-dir 1))

(use-package dired-toggle
  :bind
  ("C-c t d" . dired-toggle))

(use-package dtrt-indent ; Auto-detect indentation mode
  :defer t
  :init
  (dtrt-indent-mode)
  :config
  (setq dtrt-indent-active-mode-line-info "(⇥)")
  :commands dtrt-indent-mode)

(use-package easy-kill
  :bind
  (([remap kill-ring-save] . easy-kill)
   ([remap mark-sexp]      . easy-mark)))

(use-package evil ; VIM-behavior
  :defer t
  :init
  (evil-mode)
  :config
  (use-package evil-matchit
    :init
    (global-evil-matchit-mode))
  (use-package evil-surround
    :init
    (global-evil-surround-mode))
  (unbind-key "SPC" evil-normal-state-map)
  ;; Insert state uses Emacs key-map.
  (setq evil-insert-state-map (make-sparse-keymap))
  (define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)
  :commands evil-delay)

(use-package god-mode ; Ctrl prefix everything
  :bind
  ("C-." . god-local-mode)
  :init
  (use-package evil-god-state ; Ctrl prefix everything
    :bind
    (:map evil-normal-state-map
          ("SPC" . evil-execute-in-god-state))
    :config
    (evil-define-key 'god global-map [escape] 'evil-god-state-bail))
  :commands evil-execute-in-god-state)

(use-package golden-ratio
  :init
  (golden-ratio-mode)
  :diminish (golden-ratio-mode . " ⓖ"))

(use-package flycheck ; Linting and syntax checking
  :defer 5
  :bind
  ("C-c t f" . flycheck-mode)
  :init
  (global-flycheck-mode)
  :config
  (use-package helm-flycheck)
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-standard-error-navigation nil
        flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  :commands
  (global-flycheck-mode
   flycheck-display-error-messages-unless-error-list
   flycheck-next-error
   flycheck-previous-error))

(use-package focus-autosave-mode ; Save buffers when focus is lost
  :init
  (focus-autosave-mode)
  :commands focus-autosave-mode
  :diminish
  focus-autosave-mode)

(use-package git-messenger
  :bind
  ("C-c g m" . git-messenger:popup-message))

(use-package helm ; Completion system
  :bind
  (([remap execute-extended-command] . helm-M-x)
   ([remap switch-to-buffer]         . helm-mini)
   ([remap list-buffers]             . helm-buffers-list)
   ([remap find-files]               . helm-find-files)
   ([remap occur]                    . helm-occur)
   ([remap yank-pop]                 . helm-show-kill-ring)
   ([remap insert-register]          . helm-register)
   ("C-x f"   . helm-multi-files)
   ("C-x r"   . helm-recentf)
   ("C-c j t" . helm-imenu)
   ("C-h a"   . helm-apropos)
   ("M-H"     . helm-resume)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action)
   ("C-i"   . helm-execute-persistent-action)
   ("C-z"   . helm-select-action)
   ("A-v"   . helm-previous-page))
  :config
  (use-package helm-descbinds ; Describe key bindings
    :bind
    ("C-h b" . helm-descbinds)
    :init
    (fset 'describe-bindings 'helm-descbinds))
  (use-package helm-swoop
    :bind
    (("C-c s s" . helm-swoop)
     ("C-c s S" . helm-multi-swoop)
     ("C-c s C-s" . helm-multi-swoop-all))
    :config
    (setq helm-swoop-split-window-function #'helm-default-display-buffer)
    :commands helm-default-display-buffer)
  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-ff-file-name-history-use-recentf t
        helm-ff-search-library-in-sexp t
        helm-display-header-line nil
        helm-imenu-execute-action-at-once-if-one nil)
  (helm-autoresize-mode)
  :defines
  (helm-M-x-fuzzy-match
   helm-buffers-fuzzy-matching
   helm-recentf-fuzzy-match
   helm-imenu-fuzzy-match
   helm-ff-file-name-history-use-recentf
   helm-ff-search-library-in-sexp
   helm-display-header-line
   helm-imenu-execute-action-at-once-if-one)
  :commands
  (helm-autoresize-mode
   helm-execute-persistent-action
   helm-select-action
   helm-previous-page)
  :diminish helm-mode)

(use-package helm-make
  :bind
  (("C-c c c" . helm-make-projectile)
   ("<f5>"    . helm-make-projectile)))

(use-package hlinum
  :defer t
  :init
  (hlinum-activate)
  :config
  (set-face-attribute 'linum-highlight-face nil
                      :foreground "#AF0000"
                      :background (face-attribute 'linum :background)))

(use-package magit
  :bind
  (("C-c g c" . magit-clone)
   ("C-c g s" . magit-status)
   ("C-c g b" . magit-blame)
   ("C-c g l" . magit-log-buffer-file)
   ("C-c g p" . magit-pull))
  :init
  (global-magit-file-mode)
  :config
  (use-package evil-magit)
  (unbind-key "<C-return>" magit-file-section-map)
  (setenv "GIT_PAGER" "")
  (setq magit-save-repository-buffers 'dontask
        magit-refs-show-commit-count 'all
        magit-log-buffer-file-locked t))

(use-package restclient ; REST REPL
  :defer t
  :config
  (use-package company-restclient
    :after company
    :config
    (add-to-list 'company-backends 'company-restclient)))

(use-package page-break-lines ; Display page breaks as a horizontal line
  :defer t
  :init
  (global-page-break-lines-mode)
  :diminish page-break-lines-mode)

(use-package projectile
  :defer 5
  :bind-keymap
  ("C-c p"   . projectile-command-map)
  ("C-c C-p" . projectile-command-map)
  :init
  (projectile-global-mode)
  :config
  (use-package helm-projectile
    :config
    (setq projectile-completion-system 'helm
          helm-projectile-fuzzy-match t)
    (helm-projectile-on))
  (setq projectile-enable-caching nil
        projectile-cache-file (expand-file-name "emacs/projectile.cache" user-cache-directory)
        projectile-known-projects-file (expand-file-name "emacs/projectile-bookmarks.eld" user-cache-directory))
  :diminish projectile-mode)

(use-package stripe-buffer ; Striped directory listing
  :defer 1
  :init
  (add-hook 'dired-mode-hook #'stripe-listify-buffer)
  :config
  (set-face-attribute 'stripe-highlight nil
                      :background (face-attribute 'font-lock-comment-face :background))
  (set-face-attribute 'stripe-hl-line nil
                      :foreground (face-attribute 'default :foreground)
                      :background (face-attribute 'hl-line :background)))

(use-package sudo-edit
  :defer t
  :bind
  (("C-c f s" . sudo-edit)
   ("C-c f S" . sudo-edit-current-file)))

(use-package undo-tree
  :defer t
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-history-directory-alist `((".*" . ,undo-dir))
        undo-tree-auto-save-history t
        undo-tree-visualizer-diff t
        undo-tree-visualizer-timestamps t)
  :diminish undo-tree-mode)

(use-package visual-fill-column
  :defer t
  :init
  (setq-default visual-fill-column-width 100
                visual-fill-column-center-text t)
  (add-hook 'text-mode-hook #'visual-fill-column-mode)
  (add-hook 'prog-mode-hook #'visual-fill-column-mode))

(use-package visual-regexp-steroids
  :bind
  (([remap isearch-backward] . vr/isearch-backward)
   ([remap isearch-forward]  . vr/isearch-forward)
   ("C-c s r" . vr/query-repalce)
   ("C-c s R" . vr/replace))
  :commands pcre-to-elisp)

(use-package which-key
  :defer t
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.4
        which-key-sort-order 'which-key-prefix-then-key-order
        which-key-key-replacement-alist
        '(("<\\([[:alnum:]-]+\\)>" . "\\1")
          ("up"                    . "↑")
          ("right"                 . "→")
          ("down"                  . "↓")
          ("left"                  . "←")
          ("DEL"                   . "⌫")
          ("deletechar"            . "⌦")
          ("RET"                   . "⏎"))
        which-key-description-replacement-alist
        '(("Prefix Command" . "prefix")
          ("\\`\\?\\?\\'"   . "λ")
          ("projectile-"    . "pt-")
          ("helm-"          . "h-")))
  (which-key-declare-prefixes
    "C-c !" "flycheck"
    "C-c =" "diff"
    "C-c b" "buffers"
    "C-c f" "files"
    "C-c g" "git"
    "C-c h" "helm/help"
    "C-c j" "jump"
    "C-c p" "projects"
    "C-c s" "search"
    "C-c t" "toggle"
    "C-c w" "windows")
  (which-key-enable-god-mode-support)
  :commands which-key-enable-god-mode-support
  :diminish which-key-mode)

(use-package writeroom-mode ; Distraction-free editing
  :bind (("C-c t r" . writeroom-mode)))

(use-package zoom-window ; Temporary one window
  :bind
  ("C-c w z" . zoom-window-zoom)
  :config
  (setq zoom-window-mode-line-color "PaleGoldenrod"))

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
    (setq alchemist-test-status-modeline nil)))

(use-package erlang
  :mode ("\\.erl\\'" . erlang-mode))

(use-package fish-mode
  :mode ("\\.fish\\'" . fish-mode)
  :interpreter ("fish" . fish-mode)
  :commands fish-mode
  :init
  (setq-default indent-tabs-mode nil))

(use-package gitattributes-mode)
(use-package gitconfig-mode)
(use-package gitignore-mode)

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
         ("\\.markdown\\'"    . markdown-mode))
  :init
  (add-hook 'gfm-mode-hook #'turn-off-auto-fill)
  :config
  (setq markdown-header-scaling t
        markdown-enable-wiki-links t
        markdown-wiki-link-fontify-missing t)
  :commands (markdown-mode gfm-mode))

(use-package puppet-mode
  :mode ("\\.pp\\'" . puppet-mode))

(use-package python-mode
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

(use-package rst
  :bind
  (:map rst-mode-map
        ("M-RET" . rst-insert-list))
  :config
  (setq rst-indent-literal-minimized 3
        rst-indent-literal-normal 3))

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

(use-package sh-script
  :defer t
  :mode (("\\.sh\\'" . sh-mode)
         ("\\.zsh\\'" . sh-mode)
         ("\\.bash\\'" . sh-mode))
  :config
  (use-package company-shell
    :init
    (add-hook 'sh-mode-hook 'company-shell-mode))
  (setq-default indent-tabs-mode t
                sh-basic-offset 4
                sh-indentation 4))

(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode))
;;; init.el ends here
