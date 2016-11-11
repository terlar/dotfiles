;;; init.el --- main configuration entry point

;;; Commentary:

;; Emacs configuration of Terje Larsen.

;;; Code:
;;; -*- lexical-binding: t -*-

;; Delay garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook
  (lambda ()
    (setq gc-cons-threshold (* 100 1024 1024))))

;; Enable debug during startup
(setq debug-on-error t)
(setq debug-on-quit t)

;;; Paths
(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
  (dolist (dir load-path)
    (make-directory dir t)))

;; Cache inside XDG_CACHE_HOME/emacs
(defvar my-cache-directory
  (if (getenv "XDG_CACHE_HOME")
    (concat (getenv "XDG_CACHE_HOME") "/emacs/")
    (expand-file-name "~/.cache/emacs/")))

(defvar my-data-directory
  (if (getenv "XDG_DATA_HOME")
    (concat (getenv "XDG_DATA_HOME") "/emacs/")
    (expand-file-name "~/.local/share/emacs/")))

;; Packages inside XDG_DATA_HOME/emacs/elpa
(setq package-user-dir (concat my-data-directory "elpa"))

(defvar pcache-directory (concat my-cache-directory "pcache/"))
(unless (file-exists-p pcache-directory)
  (make-directory pcache-directory t))

(defvar undo-directory (concat my-cache-directory "undo"))
(unless (file-exists-p undo-directory)
  (make-directory undo-directory t))

;;; Package base
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-verbose nil)
(setq bind-key-describe-special-forms t)

;;; Settings

;; Encoding
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; Reduce noise
(setq ad-redefinition-action 'accept)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; Allow font-lock-mode to do background parsing
(setq jit-lock-chunk-size 1000)
(setq jit-lock-defer-time 0.05)
(setq jit-lock-stealth-time 1)

;; File
(setq auto-save-default nil)
(setq auto-save-list-file-prefix (concat my-cache-directory ".auto-saves-"))
(setq backup-inhibited t)
(setq create-lockfiles nil)
(setq find-file-visit-truename t)
(setq load-prefer-newer t)

(defun save-all ()
  "Save all files when focus is lost."
  (interactive)
  (save-some-buffers t))

(add-hook 'focus-out-hook 'save-all)

;;; Appearance
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(use-package flatui-theme)
(use-package leuven-theme :defer t)
(use-package color-theme-sanityinc-tomorrow :defer t)
(use-package twilight-bright-theme :defer t)

(set-face-attribute 'default nil
  :family "Input Mono"
  :height 120)
(set-face-attribute 'variable-pitch nil
  :family "Merriweather Sans"
  :height 120)
(copy-face 'default 'fixed-pitch)

(defun on-frame-open (&optional frame)
  "If the FRAME created in terminal don't load background color."
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)))

(add-hook 'after-make-frame-functions 'on-frame-open)

;;; Libraries
(use-package auto-compile
  :init
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;;; Enable disabled commands
(put 'downcase-region 'disabled nil) ; Let downcasing work

;;; Usability
;; Buffers
(setq uniquify-buffer-name-style 'forward)

(defun is-useful-buffer (buffer)
  "Determine if BUFFER is useful."
  (not (string-match
         "^ ?\\*.*\\*\\(<[0-9]+>\\)?$"
         (buffer-name buffer))))

(defun is-perspective-buffer (buffer)
  "Determine if BUFFER belongs to current perspective."
  (if (fboundp 'persp-buffer-list)
    (memq buffer (persp-buffer-list))
    t))

(defun is-visible-buffer (buffer)
  "Determine if BUFFER should be visible."
  (and (is-useful-buffer buffer) (is-perspective-buffer buffer)))

;; Filter out buffers that is not deemed visible.
(push '(buffer-predicate . is-visible-buffer) default-frame-alist)

;; Keep buffers opened when leaving an emacs client,
;; but kill temp buffers when done with them.
(setq-default server-kill-new-buffers nil)
(setq-default server-temp-file-regexp "^/tmp/Re\\|/draft\\|COMMIT_EDITMSG\\|PULLREQ_EDITMSG$")

;; Input
(diminish 'abbrev-mode)
(diminish 'isearch-mode)

(setq completion-cycle-threshold 5)
(setq echo-keystrokes 0.1)
(setq isearch-allow-scroll t)

;; Window behavior
(setq switch-to-buffer-preserve-window-point t)
(setq window-combination-resize t)

;; Fringe indicators
(setq-default indicate-buffer-boundaries 'right)
(setq-default indicate-empty-lines t)
(setq visual-line-fringe-indicators
  '(left-curly-arrow right-curly-arrow))

;; Visual word wrapping
(diminish 'visual-line-mode)
(global-visual-line-mode)

;; Highlight matching parenthesis
(show-paren-mode)

;; Show line and column number in the mode line
(column-number-mode)
(line-number-mode)

;;; Editing
;; Increase line spacing
(setq-default line-spacing 0.2)

;; Newline at end of file
(setq require-final-newline t)

;; Word wrapping
(diminish 'auto-fill-function " ▨")
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'prog-mode-hook 'auto-fill-mode)
(setq-default fill-column 72)

;; Indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
(setq-default tab-stop-list (number-sequence 2 80 2))

;;; Shell
;; Emacs compatible pager
(setenv "PAGER" "/usr/bin/cat")

;;; Keys
(defun simulate-key-press (key)
  "Pretend that KEY was pressed.
KEY must be given in `kbd' notation."
  `(lambda () (interactive)
     (setq prefix-arg current-prefix-arg)
     (setq unread-command-events (listify-key-sequence (read-kbd-macro ,key)))))

(defalias 'yes-or-no-p 'y-or-n-p)

(bind-key "<escape>" 'keyboard-escape-quit)

;; C-
(defun kill-region-or-backward-kill-word (&optional arg region)
  "Takes ARG and REGION and passes to:
`kill-region' if the region is active, otherwise `backward-kill-word'."
  (interactive
   (list (prefix-numeric-value current-prefix-arg) (use-region-p)))
  (if region
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

(bind-key "C-w" 'kill-region-or-backward-kill-word)

(defun toggle-tab-width-setting ()
  "Cycle 'tab-width' between values 2, 4, and 8."
  (interactive)
  (setq tab-width
    (cond
      ((eq tab-width 8) 2)
      ((eq tab-width 2) 4)
      (t 8)))
  (redraw-display)
  (message "Set tab-width to %d." tab-width))

(defun toggle-indent-mode-setting ()
  "Toggle indenting modes between tabs and spaces."
  (interactive)
  (setq indent-tabs-mode(if (eq indent-tabs-mode t) nil t))
  (message "Indenting using %s." (if (eq indent-tabs-mode t) "tabs" "spaces")))

(bind-key "C-=" 'toggle-tab-width-setting)
(bind-key "C-+" 'toggle-indent-mode-setting)

;; M-
(bind-key "M-W" 'mark-word)

(defun mark-line ()
  "Mark the current line."
  (interactive)
  (beginning-of-line)
  (let ((here (point)))
    (end-of-line)
    (set-mark (point))
    (goto-char here)))

(bind-key "M-L" 'mark-line)

(defun mark-sentence ()
  "Mark the current sentence."
  (interactive)
  (backward-sentence)
  (mark-end-of-sentence 1))

(bind-key "M-S" 'mark-sentence)
(bind-key "M-X" 'mark-sexp)
(bind-key "M-D" 'mark-defun)

(bind-key "M-g c" 'goto-char)
(bind-key "M-g l" 'goto-line)

;; C-c
(bind-key "C-c <tab>" 'ff-find-other-file) ; Open alternate file

(defun delete-current-line ()
  "Delete the current line."
  (interactive)
  (let ((here (point)))
    (beginning-of-line)
    (kill-line t)
    (goto-char here)))

(bind-key "C-c d" 'delete-current-line)
(bind-key "C-c q" 'fill-region)
(bind-key "C-c ;" 'comment-or-uncomment-region)

;; C-c t (Toggle)
(bind-keys :prefix-map toggle-map
  :prefix "C-c t"
  ("d" . toggle-debug-on-error)
  ("h" . hl-line-mode)
  ("l" . linum-mode)
  ("r" . ruler-mode)
  ;; Toggle fixed-width/variable-width
  ("v" . variable-pitch-mode))

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

;;; Builtin packages
(use-package align ; Align text
  :bind
  ( ("C-c [" . align-regexp)
    ("C-c x a a" . align)
    ("C-c x a c" . align-current))
  :init
  (defadvice align (around smart-tabs activate)
    (let ((indent-tabs-mode nil)) ad-do-it))
  (defadvice align-regexp (around smart-tabs activate)
    (let ((indent-tabs-mode nil)) ad-do-it))
  (defadvice indent-relative (around smart-tabs activate)
    (let ((indent-tabs-mode nil)) ad-do-it))
  :commands align)

(use-package autorevert ; Auto-revert buffers for changed files
  :diminish (auto-revert-mode . " ⎌")
  :ensure nil
  :config
  (setq auto-revert-verbose nil))

(use-package bookmark
  :bind
  ("C-c f b" . list-bookmarks)
  :config
  (setq bookmark-save-flag t)
  (setq bookmark-default-file (concat my-cache-directory "bookmarks")))

(use-package bug-reference ; Bug references to URL:s
  :defer t
  :init
  (add-hook 'text-mode-hook 'bug-reference-mode)
  (add-hook 'prog-mode-hook 'bug-reference-prog-mode))

(use-package calendar
  :bind
  ("C-c a c" . calendar)
  :config
  (setq calendar-week-start-day 1))

(use-package compile
  :bind
  ("C-c c C" . recompile)
  :config
  (setq compilation-always-kill t)
  (setq compilation-ask-about-save nil)
  (setq compilation-context-lines 3)
  (setq compilation-disable-input t)
  ;; Automatically scroll
  (setq compilation-scroll-output 'first-error)
  ;; Skip warnings and info messages
  (setq compilation-skip-threshold 2)

  ;; Filter ANSI escape codes in compilation-mode output
  (require 'ansi-color)
  (add-hook 'compilation-filter-hook
    (lambda ()
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region compilation-filter-start
          (point)))))
  :commands ansi-color-apply-on-region)

(use-package delsel ; Delete selection upon insert
  :defer t
  :init (delete-selection-mode))

(use-package doc-view ; Document viewer
  :defer t
  :config
  (setq doc-view-resolution 300))

(use-package ediff
  :bind
  ( ("C-c = b" . ediff-buffers)
    ("C-c = B" . ediff-buffers3)
    ("C-c = c" . compare-windows)
    ("C-c = =" . ediff-files)
    ("C-c = f" . ediff-files)
    ("C-c = F" . ediff-files3)
    ("C-c = r" . ediff-revision)
    ("C-c = p" . ediff-patch-file)
    ("C-c = P" . ediff-patch-buffer)
    ("C-c = l" . ediff-regions-linewise)
    ("C-c = w" . ediff-regions-wordwise)
    :map ediff-mode-map
    ("d" . ediff-copy-both-to-C)
    ("j" . ediff-next-difference)
    ("k" . ediff-previous-difference))
  :preface
  (defun ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
      (concat
        (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
        (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally)
  (add-hook 'ediff-quit-hook 'winner-undo)
  :commands
  ediff-copy-diff
  ediff-get-region-contents
  winner-undo)

(use-package eldoc ; Documentation in minibuffer
  :diminish (eldoc-mode . " 👓")
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
  (add-hook 'python-mode-hook 'eldoc-mode))

(use-package electric
  :defer t
  :init
  ;; Indent line after RET
  (electric-indent-mode)
  ;; Auto-insert matching delimiters
  (electric-pair-mode))

(use-package etags
  :bind
  ( ("M-T"     . tags-search)
    ("C-c s t" . tags-search))
  :config
  (setq tags-revert-without-query t))

(use-package eww
  :bind
  ( ("C-c a w w" . eww)
    ("C-c a w u" . eww-browse-url))
  :init
  (add-hook 'eww-mode-hook 'buffer-face-mode))

(use-package flyspell ; Spell-checking
  :diminish (flyspell-mode . " ∼")
  :bind
  ("C-c t s" . flyspell-mode)
  :init
  (add-hook 'message-mode-hook 'flyspell-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  :config
  (setq flyspell-use-meta-tab nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq flyspell-issue-message-flag nil))

(use-package goto-addr ; Make links clickable
  :bind
  ( ("C-c t a" . goto-address-mode)
    ("C-c t A" . goto-address-prog-mode))
  :init
  (add-hook 'prog-mode-hook 'goto-address-prog-mode)
  (add-hook 'text-mode-hook 'goto-address-mode))

(use-package hippie-exp ; Expansion and completion (line)
  :bind
  ([remap dabbrev-expand] . hippie-expand)
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
  (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook 'hl-line-mode)))

(use-package image-file ; Display images
  :defer t
  :init
  (auto-image-file-mode))

(use-package outline ; Navigate outlines in buffers
  :diminish outline-minor-mode
  :defer t
  :init
  (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook 'outline-minor-mode)))

(use-package recentf
  :defer t
  :init
  (setq recentf-save-file (concat my-cache-directory "recentf"))
  (recentf-mode)
  :config
  (setq recentf-auto-cleanup 300)
  (setq recentf-exclude
    (list "/\\.git/.*\\'" ; Git contents
      "/.cache/.*\\'"     ; Cache files
      "/elpa/.*\\'"       ; Package files
      "/ssh:"             ; SSH files
      "/tmp/"))
  (setq recentf-max-menu-items 20)
  (setq recentf-max-saved-items 200))

(use-package savehist ; Save mini buffer history
  :defer t
  :init
  (savehist-mode)
  :config
  (setq history-delete-duplicates t)
  (setq history-length t)
  (setq savehist-file (concat my-cache-directory "history"))
  (setq savehist-save-minibuffer-history t))

(use-package saveplace ; Remember point position in files
  :defer t
  :init
  (setq-default save-place t)
  :config
  (setq save-place-file (concat my-cache-directory "places"))
  (setq save-place-forget-unreadable-files nil))

(use-package smart-tabs-mode ; Tabs for indentation, spaces for alignment
  :init
  (smart-tabs-insinuate 'c++ 'c 'java 'javascript 'python)
  :commands smart-tabs-insinuate)

(use-package subword ; Recognize camel and snake case
  :diminish subword-mode
  :defer t
  :init
  (add-hook 'prog-mode-hook 'subword-mode))

(use-package shell
  :bind
  ("C-c a t" . shell))

(use-package term
  :bind
  ("C-c a T" . ansi-term))

(use-package whitespace
  :diminish (whitespace-mode)
  :bind
  ("C-c t w" . whitespace-mode)
  :config
  (setq whitespace-line-column 100))

(use-package windmove ; Move between windows
  :bind
  ( ("C-c w <left>"  . windmove-left)
    ("C-c w <right>" . windmove-right)
    ("C-c w <up>"    . windmove-up)
    ("C-c w <down>"  . windmove-down)))

(use-package winner ; Undo and redo window configuration
  :bind
  ( ("M-N" . winner-redo)
    ("M-P" . winner-undo))
  :init
  (winner-mode))

;;; Local packages
(use-package ibus
  :if (eq window-system 'x)
  :ensure nil
  :load-path "vendor/ibus/"
  :init
  (ibus-mode-on)
  :commands ibus-mode-on)

(use-package show-tab-width-mode
  :ensure nil
  :load-path "lisp/"
  :init
  (show-tab-width-mode)
  :commands show-tab-width-mode)

;;; Packages
(use-package ace-window ; Fast window switching
  :bind
  ("C-c w w" . ace-window))

(use-package adaptive-wrap ; Align wrapped lines
  :defer t
  :init
  (add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode))

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :defer 5
  :bind
  ("C-c t i" . aggressive-indent-mode)
  :init
  (global-aggressive-indent-mode 1)
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'dockerfile-mode))

(use-package anzu ; Position/matches count for search
  :diminish anzu-mode
  :bind
  ( ([remap query-replace]        . anzu-query-replace)
    ([remap query-replace-regexp] . anzu-query-replace-regexp)
    :map isearch-mode-map
    ([remap isearch-query-replace]        . anzu-isearch-query-replace)
    ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :init
  (global-anzu-mode)
  :config
  (use-package evil-anzu))

(use-package auto-dictionary ; Automatically infer dictionary
  :bind
  ( ("C-c l l" . adict-change-dictionary)
    ("C-c l g" . adict-guess-dictionary)))

(use-package bug-reference-github ; Use GitHub URL for bug reference
  :defer t
  :init
  (add-hook 'find-file-hook 'bug-reference-github-set-url-format))

(use-package company ; Completion
  :diminish company-mode
  :defer t
  :bind
  ( :map company-mode-map
    ("TAB"   . company-indent-or-complete)
    ("<tab>" . company-indent-or-complete)
    :map company-active-map
    ("C-e"       . company-complete-selection)
    ("C-f"       . company-complete-selection)
    ("TAB"       . company-complete-common-or-cycle)
    ("<tab>"     . company-complete-common-or-cycle)
    ("S-TAB"     . company-select-previous)
    ("<backtab>" . company-select-previous)
    ("RET"       . nil)
    ("<return>"  . nil)
    ("ESC"       . company-abort)
    ("<escape>"  . company-abort))
  :preface
  (defun company-indent-or-complete ()
    "Try to indent before trying to complete."
    (interactive)
    (if (looking-at "\\_>")
      (company-complete-common-or-cycle)
      (indent-according-to-mode)))
  :init
  (global-company-mode)
  :config
  (use-package company-emoji ;; Emoji-word completion
    :config
    (add-to-list 'company-backends 'company-emoji))

  (use-package company-quickhelp ;; Documentation popup
    :init
    (company-quickhelp-mode)
    :config
    (use-package pos-tip))

  (use-package company-statistics ;; History based sorting
    :init
    (setq company-statistics-file
      (concat my-cache-directory "company-statistics-cache.el"))
    (company-statistics-mode))

  (setq company-echo-delay 0)
  (setq company-idle-delay .3)
  (setq company-minimum-prefix-length 2)
  (setq company-require-match 'never)
  (setq company-selection-wrap-around t)
  (setq company-show-numbers t)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t))

(use-package default-text-scale ; Text scale for all buffers
  :bind
  ( ([C-mouse-4] . default-text-scale-increase)
    ([C-mouse-5] . default-text-scale-decrease)
    ("C-M-=" . default-text-scale-increase)
    ("C-M--" . default-text-scale-decrease)))

(use-package deft ; Notes managing
  :config
  (setq deft-directory "~/notes")
  (setq deft-extensions '("txt" "tex" "org" "md" "rst")))

(use-package diff-hl
  :init
  (global-diff-hl-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

(use-package dired+
  :defer 1
  :init
  (diredp-toggle-find-file-reuse-dir 1)
  :config
  (setq diredp-image-preview-in-tooltip 300)
  ;; Remove color and font decoration
  (setq font-lock-maximum-decoration '((dired-mode) (t . t))))

(use-package dired-toggle
  :bind
  ("C-c t d" . dired-toggle))

(use-package easy-kill
  :bind
  (([remap kill-ring-save] . easy-kill)
   ([remap mark-sexp]      . easy-mark)))

(use-package editorconfig
  :init
  (add-hook 'prog-mode-hook (editorconfig-mode))
  (add-hook 'text-mode-hook (editorconfig-mode))
  :diminish (editorconfig-mode . " ⚙"))

(use-package evil ; VIM-behavior
  :defer t
  :bind
  ( :map evil-normal-state-map
    ("<tab>" . next-buffer)
    ("<backtab>" . previous-buffer))
  :init
  (evil-mode)
  (add-hook 'after-change-major-mode-hook
    (lambda ()
      (setq evil-shift-width tab-width)))
  :config
  (use-package evil-matchit
    :init
    (global-evil-matchit-mode))
  (use-package evil-surround
    :init
    (global-evil-surround-mode))
  (define-key evil-normal-state-map (kbd "SPC") (simulate-key-press "C-c"))
  (define-key evil-visual-state-map (kbd "SPC") (simulate-key-press "C-c"))
  ;; Insert state uses Emacs key-map.
  (setq evil-insert-state-map (make-sparse-keymap))
  (define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)
  (setq evil-want-fine-undo 'fine
    evil-auto-indent t)
  :commands evil-delay)

(use-package fic-mode
  :init
  (add-hook 'prog-mode-hook 'fic-mode)
  :config
  (set-face-attribute 'fic-face nil
                      :weight 'bold
                      :background (face-attribute 'font-lock-comment-face :background)
                      :foreground (face-attribute 'font-lock-comment-face :foreground)))

(use-package flycheck ; Linting and syntax checking
  :defer 5
  :bind
  (("C-c t f" . flycheck-mode)
    ("M-n" . flycheck-next-error)
    ("M-p" . flycheck-previous-error))
  :init
  (global-flycheck-mode)
  :config
  (use-package helm-flycheck)
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-standard-error-navigation nil
    flycheck-display-errors-function 'flycheck-display-error-messages-unless-error-list)
  :commands
  (global-flycheck-mode
    flycheck-display-error-messages-unless-error-list
    flycheck-next-error
    flycheck-previous-error))

(use-package super-save ; Save buffers when focus is lost
  :init
  (super-save-mode +1)
  :diminish super-save-mode)

(use-package git-messenger
  :bind
  ("C-c g m" . git-messenger:popup-message))

(use-package git-timemachine)

(use-package github-browse-file
  :commands github-browse-file)

(use-package god-mode ; Ctrl prefix everything
  :bind
  ("C-c SPC" . god-local-mode)
  :init
  (use-package evil-god-state ; Ctrl prefix everything
    :bind
    (""
     :map evil-normal-state-map
     ("C-c SPC" . evil-execute-in-god-state))
    :config
    (evil-define-key 'god global-map [escape] 'evil-god-state-bail)
    :commands evil-execute-in-god-state))

(use-package helm ; Completion system
  :bind
  (([remap execute-extended-command] . helm-M-x)
    ([remap switch-to-buffer]         . helm-mini)
    ([remap list-buffers]             . helm-buffers-list)
    ([remap find-file]                . helm-find-files)
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
    ("C-e"   . helm-execute-persistent-action)
    ("C-z"   . helm-select-action)
    ("A-v"   . helm-previous-page))
  :init
  (helm-mode)
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
    (setq helm-swoop-split-window-function 'helm-default-display-buffer)
    :commands helm-default-display-buffer)
  (use-package helm-systemd)
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

(use-package highlight-indent-guides
  :config
  (set-face-foreground 'highlight-indent-guides-character-face
                       (face-attribute 'fringe :background))
  (setq highlight-indent-guides-method 'character))

(use-package hlinum
  :defer t
  :init
  (hlinum-activate)
  :config
  (set-face-attribute 'linum-highlight-face nil
                      :foreground "#AF0000"
                      :background (face-attribute 'linum :background)))

(use-package ignoramus ; Ignore files
  :config
  ;; Ignore some additional directories
  (dolist (name '(".vagrant"))
    (add-to-list 'ignoramus-file-basename-exact-names name))

  (ignoramus-setup))

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
  (use-package magit-gh-pulls
    :init
    (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))
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

(use-package persp-mode
  :bind
  (("M-[" . persp-prev)
    ("M-]" . persp-next))
  :init
  (setq persp-save-dir (concat my-cache-directory "persp-confs/"))
  (persp-mode)
  :config
  (set-face-attribute 'persp-face-lighter-buffer-not-in-persp nil
    :background (face-attribute 'isearch-fail :background)
    :foreground (face-attribute 'isearch-fail :foreground))

  (setq persp-autokill-buffer-on-remove 'kill-weak)

  (defvar after-find-file-hook nil)
  (advice-add 'find-file :after (lambda (&rest args) (run-hooks 'after-find-file-hook)))

  (def-auto-persp "projectile"
    :parameters '((dont-save-to-file . t))
    :hooks '(after-find-file-hook)
    :switch 'frame
    :predicate
    (lambda (buffer)
      (when (and (buffer-file-name)
              (projectile-project-p))
        t))
    :get-name-expr
    (lambda ()
      (projectile-project-name)))

  (setq persp-add-buffer-on-find-file 'if-not-autopersp)
  (add-hook 'persp-after-load-state-functions
    '(lambda (&rest args) (persp-auto-persps-pickup-buffers)) t)
  :commands
  persp-auto-persps-pickup-buffers
  projectile-project-p
  projectile-project-name)

(use-package projectile
  :defer 5
  :bind-keymap
  (("C-c p" . projectile-command-map))
  :init
  (setq projectile-cache-file
    (concat my-cache-directory "projectile.cache"))
  (setq projectile-known-projects-file
    (concat my-cache-directory "projectile-bookmarks.eld"))
  (projectile-mode)
  :config
  (use-package helm-projectile
    :bind
    (("C-c s g" . helm-projectile-grep))
    :init
    (helm-projectile-on)
    :config
    (setq projectile-completion-system 'helm)
    (setq helm-projectile-fuzzy-match t))
  :diminish
  projectile-mode)

(use-package stripe-buffer ; Striped directory listing
  :defer 1
  :init
  (add-hook 'dired-mode-hook 'stripe-listify-buffer)
  :config
  (set-face-attribute 'stripe-highlight nil
    :background (face-attribute 'font-lock-comment-face :background))
  (set-face-attribute 'stripe-hl-line nil
    :foreground (face-attribute 'default :foreground)
    :background (face-attribute 'hl-line :background)))

(use-package sudo-edit
  :defer t
  :bind
  ("C-c f s" . sudo-edit))

(use-package undo-tree
  :diminish undo-tree-mode
  :bind
  ("C-c u" . undo-tree-visualize)
  :init
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist `((".*" . ,undo-directory)))
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t))

(use-package visual-fill-column
  :defer t
  :init
  (setq-default visual-fill-column-width 100
    visual-fill-column-center-text t)
  (add-hook 'text-mode-hook 'visual-fill-column-mode)
  (add-hook 'prog-mode-hook 'visual-fill-column-mode))

(use-package visual-regexp-steroids
  :bind
  (([remap isearch-backward] . vr/isearch-backward)
   ([remap isearch-forward]  . vr/isearch-forward)
   ("C-c s r" . vr/query-repalce)
   ("C-c s R" . vr/replace)))

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
          ("helm-"          . "h-")
          ("flycheck-"      . "flyc-")))
  (which-key-add-key-based-replacements
    "C-c !" "flycheck"
    "C-c =" "diff"
    "C-c @" "outline"
    "C-c a" "apps"
    "C-c a w" "web"
    "C-c b" "buffers"
    "C-c c" "compile"
    "C-c f" "files"
    "C-c g" "git"
    "C-c h" "helm/help"
    "C-c j" "jump"
    "C-c l" "language"
    "C-c p" "projects"
    "C-c s" "search"
    "C-c t" "toggles"
    "C-c w" "windows"
    "C-c x" "text"
    "C-c x a" "align")
  (which-key-enable-god-mode-support)
  :commands which-key-enable-god-mode-support
  :diminish which-key-mode)

(use-package whitespace-cleanup-mode ; Cleanup whitespace on save
  :bind
  ("C-c t c" . whitespace-cleanup-mode)
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook 'whitespace-cleanup-mode))
  :diminish (whitespace-cleanup-mode . " Ⓦ"))

(use-package writeroom-mode ; Distraction-free editing
  :bind
  ("C-c t r" . writeroom-mode))

(use-package zoom-window ; Temporary one window
  :bind
  ("C-c w z" . zoom-window-zoom)
  :config
  (setq zoom-window-mode-line-color "PaleGoldenrod"))

;;; Language packages
(use-package css-mode
  :mode "\\.css\\'")

(use-package dockerfile-mode
  :init
  (add-hook 'dockerfile-mode-hook
    (lambda ()
      (setq tab-width 2
        indent-tabs-mode nil))))

(use-package alchemist
  :mode ("\\.exs?\\'" "mix\\.lock\\'")
  :init
  (add-hook 'elixir-mode-hook 'alchemist-mode)
  (add-hook 'elixir-mode-hook 'flycheck-mode)
  :diminish alchemist-mode)

(use-package bats-mode)

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package erlang
  :mode ("\\.erl\\'" . erlang-mode)
  :config
  (use-package edts))

(use-package fish-mode
  :mode ("\\.fish\\'" . fish-mode)
  :interpreter ("fish" . fish-mode)
  :init
  (add-hook 'fish-mode-hook
    (lambda ()
      (add-hook 'before-save-hook 'fish_indent-before-save)
      (setq tab-width 4
        indent-tabs-mode nil)))
  :commands fish-mode)

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
  (use-package go-errcheck)
  (use-package go-eldoc
    :init
    (add-hook 'go-mode-hook 'go-eldoc-setup)
    :commands
    go-eldoc-setup)
  (use-package company-go
    :config
    (add-to-list 'company-backends 'company-go))

  (add-hook 'go-mode-hook 'my-go-mode-hook))

(use-package haskell-mode
  :defer t
  :bind
  (""
    :map haskell-mode-map
    ("C-c c c" . haskell-compile)
    ("<f5>"    . haskell-compile))
  :diminish interactive-haskell-mode
  :preface
  (defun haskell-setup ()
    "Setup Haskell mode."
    (setq indent-tabs-mode nil
      tab-width 4
      haskell-indent-spaces 4
      haskell-indentation-layout-offset 4
      haskell-indentation-left-offset 4))
  :init
  (add-hook 'haskell-mode-hook 'haskell-setup)
  :config
  ;; Use interpreter "stack ghci"
  (setq haskell-process-type 'stack-ghci)

  (setq haskell-notify-p t
    haskell-stylish-on-save t
    haskell-tags-on-save nil
    haskell-interactive-mode-include-file-name nil
    haskell-interactive-mode-eval-mode 'haskell-mode
    haskell-process-auto-import-loaded-modules t
    haskell-process-show-debug-tips nil
    haskell-process-suggest-haskell-docs-imports t
    haskell-process-suggest-hoogle-imports nil
    haskell-process-suggest-remove-import-lines t
    haskell-process-use-presentation-mode t)

  (remove-hook 'haskell-mode-hook 'interactive-haskell-mode)

  (use-package hi2
    :diminish (hi2-mode . " ⇥")
    :init
    (defvaralias 'hi2-ifte-offset 'haskell-indentation-left-offset)
    (defvaralias 'hi2-layout-offset 'haskell-indentation-layout-offset)
    (defvaralias 'hi2-left-offset 'haskell-indentation-left-offset)

    (setq hi2-show-indentations nil)
    (add-hook 'haskell-mode-hook 'turn-on-hi2))

  (use-package company-ghci
    :init
    (add-to-list 'company-backends 'company-ghci))


  (use-package intero
    :diminish (intero-mode . " λ")
    :init
    (add-hook 'haskell-mode-hook 'intero-mode)
    :config
    (setq haskell-process-args-stack-ghci '("--ghc-options=-ferror-spans" "--with-ghc=intero")))

  (use-package shm
    :preface
    (defun haskell-shm-setup ()
      "Setup SHM mode."
      (defvaralias 'shm-indent-spaces 'haskell-indentation-layout-offset)
      (structured-haskell-mode)
      (hl-line-mode -1))
    :init
    (add-hook 'haskell-mode-hook 'haskell-shm-setup)
    (add-hook 'haskell-interactive-mode-hook 'structured-haskell-repl-mode)
    :config
    (setq shm-auto-insert-bangs t
      shm-auto-insert-skeletons t
      shm-indent-point-after-adding-where-clause t
      shm-use-hdevtools t
      shm-use-presentation-mode t)
    (set-face-background 'shm-current-face (face-attribute 'hl-line :background))
    (set-face-background 'shm-quarantine-face "#fff0f0")
    :commands
    structured-haskell-mode
    structured-haskell-repl-mode)

  (use-package flycheck-haskell
    :commands flycheck-haskell-setup))


(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (defvaralias 'js-indent-level 'tab-width)
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-highlight-level 3))

(use-package json-mode
  :mode "\\.json\\'")

(use-package markdown-mode
  :mode
  (("\\`README\\.md\\'" . gfm-mode)
    ("\\.md\\'"          . markdown-mode)
    ("\\.markdown\\'"    . markdown-mode))
  :init
  (add-hook 'gfm-mode-hook 'turn-off-auto-fill)
  :config
  (setq markdown-header-scaling t
    markdown-enable-wiki-links t
    markdown-wiki-link-fontify-missing t)
  :commands (markdown-mode gfm-mode))

(use-package pkgbuild-mode
  :mode "/PKGBUILD\\'")

(use-package plantuml-mode
  :mode
  (("\\.puml\\'" . plantuml-mode)
   ("\\.plantuml\\'" . plantuml-mode))
  :init
  (setq plantuml-java-command "java-headless"
        plantuml-jar-path "/opt/plantuml/plantuml.jar"))

(use-package protobuf-mode)

(use-package puppet-mode
  :mode
  (("\\.pp\\'"    . puppet-mode)
   ("Puppetfile$" . puppet-mode)))

(use-package python-mode
  :init
  (add-hook 'python-mode-hook 'python-setup)
  :preface
  (defun python-setup ()
    "Setup Python mode."
    (highlight-indent-guides-mode))
  :config
  (use-package anaconda-mode
    :defer t
    :init
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
    :config
    (use-package company-anaconda
      :init
      (add-to-list 'company-backends 'company-anaconda)))
  (defvaralias 'python-indent 'tab-width)
  (setq py-indent-tabs-mode t))

(use-package raml-mode
  :ensure nil
  :load-path "vendor/raml-mode/")

(use-package rst
  :mode
  (("\\.rst\\'" . rst-mode)
   ("\\.text$"  . rst-mode))
  :bind
  (""
   :map rst-mode-map
   ("M-RET" . rst-insert-list))
  :config
  (setq rst-indent-literal-minimized 3
        rst-indent-literal-normal 3)

  (set-face-attribute 'rst-adornment nil
                      :strike-through "black"
                      :foreground (face-attribute 'default :background)
                      :background (face-attribute 'default :background))

  ;; Header scaling
  (set-face-attribute 'rst-level-1 nil
                      :background nil
                      :inherit 'variable-pitch
                      :height 1.8)
  (set-face-attribute 'rst-level-2 nil
                      :background nil
                      :inherit 'variable-pitch
                      :height 1.4)
  (set-face-attribute 'rst-level-3 nil
                      :background nil
                      :inherit 'variable-pitch
                      :height 1.2)
  (set-face-attribute 'rst-level-4 nil
                      :background nil
                      :inherit 'variable-pitch
                      :height 1.0)
  (set-face-attribute 'rst-level-5 nil
                      :background nil
                      :inherit 'variable-pitch
                      :height 1.0)
  (set-face-attribute 'rst-level-6 nil
                      :background nil
                      :inherit 'variable-pitch
                      :height 1.0))

(use-package enh-ruby-mode
  :defer t
  :mode
  (("\\.rb\\'"     . enh-ruby-mode)
   ("Gemfile$"     . enh-ruby-mode)
   ("[Rr]akefile$" . enh-ruby-mode))
  :interpreter "pry"
  :config
  (setq enh-ruby-deep-indent-paren nil)
  (use-package yari)
  (use-package robe
    :init
    (add-hook 'enh-ruby-mode-hook 'robe-mode)
    (add-to-list 'company-backends 'company-robe)
    :diminish robe-mode)
  (use-package inf-ruby
    :init
    (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)
    :config
    (use-package company-inf-ruby
      :config
      (add-to-list 'company-backends 'company-inf-ruby))
    :commands inf-ruby)
  (use-package rubocop
    :init
    (add-hook 'enh-ruby-mode-hook 'rubocop-mode))
  (use-package minitest
    :init
    (add-hook 'enh-ruby-mode-hook 'minitest-mode)
    :diminish minitest-mode)
  (use-package rspec-mode
    :init
    (add-hook 'enh-ruby-mode-hook 'rspec-mode)
    :diminish rspec-mode))

(use-package rust-mode
  :defer t
  :mode ("\\.rust\\'" . rust-mode)
  :config
  (use-package flycheck-rust
    :init
    (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))
  (use-package racer
    :init
    (add-hook 'rust-mode-hook 'racer-mode)
    :config
    (use-package company-racer
      :config
      (add-to-list 'company-backends 'company-racer))))

(use-package sh-script
  :defer t
  :mode
  (("\\.sh\\'"   . sh-mode)
   ("\\.zsh\\'"  . sh-mode)
   ("\\.bash\\'" . sh-mode))
  :init
  (defvaralias 'sh-basic-offset 'tab-width)
  (defvaralias 'sh-indentation 'tab-width)
  :config
  (set-face-attribute 'sh-quoted-exec nil
                      :background (face-attribute 'font-lock-builtin-face :background)
                      :foreground (face-attribute 'font-lock-builtin-face :foreground)))

(use-package slim-mode)

(use-package sql
  :bind
  (("C-c a s" . sql-connect)
   :map sql-mode-map
   ("C-c m p" . sql-set-product)))

(use-package swift-mode
  :config
  (with-eval-after-load 'flycheck (add-to-list 'flycheck-checkers 'swift)))

(use-package systemd)

(use-package thrift
  :defer t
  :init
  (put 'thrift-indent-level 'safe-local-variable 'integerp)
  :config
  (add-hook 'thrift-mode-hook (lambda () (run-hooks 'prog-mode-hook))))

(use-package web-mode
  :mode
  (("\\.html?\\'"      . web-mode)
   ("\\.phtml\\'"      . web-mode)
   ("\\.tpl\\.php\\'"  . web-mode)
   ("\\.jsp\\'"        . web-mode)
   ("\\.hbs\\'"        . web-mode)
   ("\\.as[cp]x\\'"    . web-mode)
   ("\\.erb\\'"        . web-mode)
   ("\\.eex\\'"        . web-mode)
   ("\\.mustache\\'"   . web-mode)
   ("\\.handlebars\\'" . web-mode)
   ("\\.djhtml\\'"     . web-mode)
   ("\\.tsx\\'"        . web-mode))
  :config
  (use-package emmet-mode
    :init
    (add-hook 'web-mode-hook 'emmet-mode)
    (setq emmet-indentation 2))
  (use-package company-web
    :config
    (add-to-list 'company-backends 'company-web-html))
  (setq web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-markup-indent-offset 2)
  (setq web-mode-style-padding 0
        web-mode-script-padding 0
        web-mode-enable-current-element-highlight t)

  (set-face-attribute 'web-mode-current-element-highlight-face nil
                      :foreground "#AF0000"
                      :background nil))

(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode)
  :init
  (add-hook 'yaml-mode-hook
    (lambda ()
      (setq tab-width 2
        indent-tabs-mode nil)))
  :config
  (use-package ansible-doc
    :defer t
    :init
    (add-hook 'yaml-mode-hook 'ansible-doc-mode)
    :diminish ansible-doc-mode)
  (use-package company-ansible
    :init
    (add-to-list 'company-backends 'company-ansible)))

(use-package company-shell
  :after company
  :config
  (setq company-shell-delete-duplicates t)
  (add-to-list 'company-backends 'company-shell)
  (add-to-list 'company-backends 'company-fish-shell))

;;; Modeline
(setq-default
 mode-line-format
 '("%e"
   mode-line-front-space
   tab-width-mode
   mode-line-mule-info
   mode-line-client
   mode-line-modified
   mode-line-remote
   mode-line-frame-identification
   mode-line-buffer-identification
   " "
   mode-line-position
   evil-mode-line-tag
   (vc-mode vc-mode)
   " "
   mode-line-modes
   mode-line-misc-info
   mode-line-end-spaces))

(setq debug-on-error nil
      debug-on-quit nil)

(setq custom-file (concat my-data-directory "custom.el"))
(load custom-file)
;;; init.el ends here
