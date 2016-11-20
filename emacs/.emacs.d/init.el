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

;; Start measuring startup time
(defconst emacs-start-time (current-time))
(unless noninteractive
  (message "Loading %s..." load-file-name))

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
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-verbose nil)
(setq bind-key-describe-special-forms t)

;;; Settings

;; Encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

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
(diminish 'auto-fill-function " ☰")
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
  :defer t
  :init
  (global-auto-revert-mode +1)
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

  (add-hook 'compilation-finish-functions
    (lambda (buf str)
      (switch-to-buffer-other-window "*compilation*")
      (read-only-mode)
      (goto-char (point-max))
      (local-set-key (kbd "q")
        (lambda ()
          (interactive)
          (quit-restore-window)))))

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
    ("C-c = w" . ediff-regions-wordwise))
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally)
  (add-hook 'ediff-quit-hook 'winner-undo)
  ;; Setting up the mappings through the bind command will leave them
  ;; behind, breaking all further modes.
  (add-hook 'ediff-keymap-setup-hook 'add-mappings-to-ediff-mode-map)
  :preface
  ;; TODO: Check if smerge-keep-all can be sufficient instead.
  (defun ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
      (concat
        (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
        (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
  (defun add-mappings-to-ediff-mode-map ()
    (define-key ediff-mode-map "d" 'ediff-copy-both-to-C)
    (define-key ediff-mode-map "j" 'ediff-next-difference)
    (define-key ediff-mode-map "k" 'ediff-previous-difference))
  :commands
  ediff-copy-diff
  ediff-get-region-contents
  winner-undo)

(use-package eldoc ; Documentation in minibuffer
  :diminish (eldoc-mode . " ℹ")
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode))

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
  :diminish (flyspell-mode . " ≁")
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
  (defvar recentf-save-file (concat my-cache-directory "recentf"))
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
  (smart-tabs-insinuate 'c++ 'c 'java 'javascript 'python 'ruby)
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
  :diminish (whitespace-mode . " ␠")
  :defer t
  :bind
  ("C-c t w" . whitespace-mode)
  :config
  (setq whitespace-line-column 100)
  (setq whitespace-style '(face spaces tabs trailing empty space-mark tab-mark)))

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
(use-package raml-mode
  :defer t
  :ensure nil
  :load-path "vendor/raml-mode/")

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
  (global-aggressive-indent-mode)
  :config
  ;; Disabled modes
  (add-to-list 'aggressive-indent-excluded-modes 'dockerfile-mode)
  ;; Disabled commands
  (dolist (command '(evil-undo-pop ws-butler-clean-region))
    (add-to-list 'aggressive-indent-protected-commands command)))

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

;; Completion support
(use-package company
  :diminish company-mode
  :defer t
  :bind
  ( :map company-mode-map
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
    ("<escape>"  . company-abort))
  :init
  (global-company-mode)
  :preface
  (defun company-indent-or-complete ()
    "Try to indent before trying to complete."
    (interactive)
    (if (looking-at "\\_>")
      (company-complete-common-or-cycle)
      (indent-according-to-mode)))
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

(use-package diff-hl ; Display diff changes in fringe
  :init
  (global-diff-hl-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

(use-package dired+ ; Dired enhancements
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

(use-package easy-kill ; Copy and select things
  :bind
  ( ([remap kill-ring-save] . easy-kill)
    ([remap mark-sexp]      . easy-mark)))

(use-package editorconfig
  :if (executable-find "editorconfig")
  :diminish (editorconfig-mode . " ⚙")
  :mode ("\\.editorconfig\\'" . conf-unix-mode)
  :init
  (add-hook 'prog-mode-hook (editorconfig-mode 1))
  (add-hook 'text-mode-hook (editorconfig-mode 1))
  :config
  (add-hook 'editorconfig-custom-hooks
    (lambda (props)
      "Use ws-butler mode instead of delete-trailing-whitespace."
      (if (equal (gethash 'trim_trailing_whitespace props) "true")
        (progn
          (setq write-file-functions
            (delete
              'delete-trailing-whitespace
              write-file-functions))
          (ws-butler-mode 1))
        (ws-butler-mode -1)))))

(use-package evil ; VIM-behavior
  :defer t
  :bind
  ( :map evil-normal-state-map
    ("<tab>"     . next-buffer)
    ("<backtab>" . previous-buffer))
  :init
  (add-hook 'after-change-major-mode-hook
    (lambda ()
      (setq evil-shift-width tab-width)))
  (evil-mode)
  :config
  (use-package evil-matchit
    :init
    (global-evil-matchit-mode))

  (use-package evil-surround
    :init
    (global-evil-surround-mode))

  ;; Map SPC to C-c in non-insert modes
  (define-key evil-normal-state-map (kbd "SPC") (simulate-key-press "C-c"))
  (define-key evil-visual-state-map (kbd "SPC") (simulate-key-press "C-c"))

  ;; Insert state uses Emacs key-map.
  (setq evil-insert-state-map (make-sparse-keymap))
  (define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)

  (setq evil-auto-indent t)
  (setq evil-want-fine-undo 'fine)
  :commands evil-delay)

(use-package fic-mode ; Highlight TODO inside comments and strings
  :init
  (add-hook 'prog-mode-hook 'fic-mode)
  :config
  (set-face-attribute 'fic-face nil
    :background (face-attribute 'font-lock-string-face :background)
    :foreground (face-attribute 'font-lock-string-face :foreground)))

(use-package flycheck ; Linting and syntax checking
  :defer 5
  :functions (flycheck-display-error-messages-unless-error-list)
  :commands (flycheck)
  :bind
  ( ("C-c t f" . flycheck-mode)
    ("M-n"     . flycheck-next-error)
    ("M-p"     . flycheck-previous-error))
  :init
  (global-flycheck-mode)
  :config
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (setq flycheck-standard-error-navigation nil)

  (use-package flycheck-pos-tip
    :after flycheck
    :commands (flycheck-pos-tip-mode)
    :config
    (flycheck-pos-tip-mode))

  (use-package helm-flycheck))

(use-package super-save ; Save buffers when focus is lost
  :diminish super-save-mode
  :init
  (super-save-mode +1))

(use-package git-messenger ; Git commit pop-up
  :bind
  ("C-c g m" . git-messenger:popup-message))

(use-package git-timemachine ; Step through historic file versions
  :bind
  ("C-c g t" . git-timemachine-toggle))

(use-package github-browse-file ; Open file on GitHub
  :bind
  ( ("C-c g h s" . github-browse-file)
    ("C-c g h b" . github-browse-file-blame))
  :commands github-browse-file)

(use-package god-mode ; Ctrl prefix everything
  :bind
  ("C-c SPC" . god-local-mode)
  :init
  (use-package evil-god-state
    :bind
    ( :map evil-normal-state-map
      ("C-c SPC" . evil-execute-in-god-state))
    :config
    (evil-define-key 'god global-map [escape] 'evil-god-state-bail)
    :commands evil-execute-in-god-state))

;; Completion system
(use-package helm
  :diminish (helm-mode)
  :defines
  ( helm-M-x-fuzzy-match
    helm-buffers-fuzzy-matching
    helm-recentf-fuzzy-match
    helm-imenu-fuzzy-match
    helm-ff-file-name-history-use-recentf
    helm-ff-search-library-in-sexp
    helm-display-header-line
    helm-imenu-execute-action-at-once-if-one)
  :commands
  ( helm-autoresize-mode
    helm-execute-persistent-action
    helm-select-action
    helm-previous-page)
  :bind
  ( ([remap execute-extended-command] . helm-M-x)
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
  (progn
    (helm-mode)
    (helm-autoresize-mode))
  :config
  (progn
    (setq helm-M-x-fuzzy-match t)
    (setq helm-buffers-fuzzy-matching t)
    (setq helm-display-header-line nil)
    (setq helm-ff-file-name-history-use-recentf t)
    (setq helm-ff-search-library-in-sexp t)
    (setq helm-imenu-execute-action-at-once-if-one nil)
    (setq helm-imenu-fuzzy-match t)
    (setq helm-recentf-fuzzy-match t)

    ;; Describe key bindings
    (use-package helm-descbinds
      :bind
      ("C-h b" . helm-descbinds)
      :init
      (fset 'describe-bindings 'helm-descbinds))

    (use-package helm-make
      :bind
      ( ("C-c c c" . helm-make-projectile)
        ("<f5>"    . helm-make-projectile)))

    (use-package helm-swoop ; Squeezed line navigation
      :bind
      ( ("C-c s s"   . helm-swoop)
        ("C-c s S"   . helm-multi-swoop)
        ("C-c s C-s" . helm-multi-swoop-all))
      :config
      (setq helm-swoop-split-window-function 'helm-default-display-buffer)
      :commands helm-default-display-buffer)

    (use-package helm-systemd
      :bind
      ("C-c a d" . helm-systemd))))

;; Indentation guides
(use-package highlight-indent-guides
  :init
  (add-hook 'emacs-lisp-mode-hook 'highlight-indent-guides-mode)
  :config
  (set-face-foreground 'highlight-indent-guides-character-face
    (face-attribute 'fringe :background))

  (setq highlight-indent-guides-method 'character))

;; Highlight current line number
(use-package hlinum
  :defer t
  :init
  (hlinum-activate)
  :config
  (set-face-attribute 'linum-highlight-face nil
    :foreground "#AF0000"
    :background (face-attribute 'linum :background)))

;; Ignore files
(use-package ignoramus
  :config
  (progn
    ;; Ignore some additional directories
    (dolist (name '("node_modules" "vendor"))
      (add-to-list 'ignoramus-file-basename-exact-names name))
    (ignoramus-setup)))

;; Git tools
(use-package magit
  :bind
  ( ("C-c g c" . magit-clone)
    ("C-c g s" . magit-status)
    ("C-c g b" . magit-blame)
    ("C-c g l" . magit-log-buffer-file)
    ("C-c g p" . magit-pull))
  :init
  (global-magit-file-mode)
  :config
  (progn
    (setenv "GIT_PAGER" "")

    (setq magit-log-buffer-file-locked t)
    (setq magit-refs-show-commit-count 'all)
    (setq magit-save-repository-buffers 'dontask)

    (use-package evil-magit)
    (use-package magit-gh-pulls
      :init
      (add-hook 'magit-mode-hook #'turn-on-magit-gh-pulls))))


;; REST REPL
(use-package restclient
  :defer t
  :config
  (use-package company-restclient
    :preface
    (progn
      (autoload 'company-mode "company")
      (defun my-restclient-company-setup ()
        (setq-local company-backends '(company-restclient))
        (company-mode)))
    :config
    (add-hook 'restclient-mode-hook #'my-restclient-company-setup)))

;; Display page breaks as a horizontal line
(use-package page-break-lines
  :diminish (page-break-lines-mode)
  :defer t
  :init
  (global-page-break-lines-mode))

;; Workspaces with buffer isolation
(use-package persp-mode
  :commands
  ( persp-mode
    persp-switch persp-prev persp-next
    def-auto-persp
    persp-auto-persps-pickup-buffers
    projectile-project-name projectile-project-p)
  :bind
  ( ("C-c o" . persp-switch)
    ("M-["   . persp-prev)
    ("M-]"   . persp-next))
  :init
  (progn
    (setq persp-save-dir (concat my-cache-directory "persp-confs/"))
    (persp-mode))
  :config
  (progn
    (setq persp-add-buffer-on-find-file 'if-not-autopersp)
    (setq persp-autokill-buffer-on-remove 'kill-weak)

    (add-hook 'persp-after-load-state-functions
      (lambda (&rest args) (persp-auto-persps-pickup-buffers)) t)

    (set-face-attribute 'persp-face-lighter-buffer-not-in-persp nil
      :background (face-attribute 'isearch-fail :background)
      :foreground (face-attribute 'isearch-fail :foreground))

    (defvar after-find-file-hook nil)
    (advice-add 'find-file :after
      (lambda (&rest args) (run-hooks 'after-find-file-hook)))

    (def-auto-persp "projectile"
      :parameters '((dont-save-to-file . t))
      :hooks '(after-find-file-hook)
      :switch 'frame
      :predicate
      '((lambda (_buffer)
          (when (and (buffer-file-name) (projectile-project-p))
            t)))
      :get-name-expr
      (lambda ()
        (projectile-project-name)))))

;; Project interaction and navigation
(use-package projectile
  :diminish (projectile-mode)
  :defer 5
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (progn
    (setq projectile-cache-file
      (concat my-cache-directory "projectile.cache"))
    (setq projectile-known-projects-file
      (concat my-cache-directory "projectile-bookmarks.eld"))
    (projectile-mode))
  :config
  (add-to-list 'projectile-globally-ignored-directories ".cache")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "tmp")
  (add-to-list 'projectile-globally-ignored-directories "vendor")

  ;; Completion system
  (use-package helm-projectile
    :bind
    ("C-c s g" . helm-projectile-grep)
    :init
    (helm-projectile-on)
    :config
    (setq helm-projectile-fuzzy-match t)
    (setq projectile-completion-system 'helm)))

;; Striped directory listing
(use-package stripe-buffer
  :defer 1
  :init
  (add-hook 'dired-mode-hook 'stripe-listify-buffer)
  :config
  (set-face-attribute 'stripe-highlight nil
    :background (face-attribute 'fringe :background)))

;; Utilities for opening files with sudo
(use-package sudo-edit
  :defer t
  :bind
  ("C-c f s" . sudo-edit))

;; Tree based undo history
(use-package undo-tree
  :diminish (undo-tree-mode)
  :bind
  ("C-c u" . undo-tree-visualize)
  :init
  (progn
    (setq undo-tree-auto-save-history t)
    (setq undo-tree-history-directory-alist `((".*" . ,undo-directory)))
    (global-undo-tree-mode))
  :config
  (progn
    (setq undo-tree-visualizer-diff t)
    (setq undo-tree-visualizer-timestamps t)))

;; Wrap lines at fill-column
(use-package visual-fill-column
  :defer t
  :init
  (progn
    (setq-default visual-fill-column-center-text t)
    (setq-default visual-fill-column-width 100)
    (add-hook 'text-mode-hook 'visual-fill-column-mode)
    (add-hook 'prog-mode-hook 'visual-fill-column-mode)))

;; Use of modern regexp engines
(use-package visual-regexp-steroids
  :commands (vr/replace vr/query-repalce)
  :bind
  ( ([remap isearch-backward] . vr/isearch-backward)
    ([remap isearch-forward]  . vr/isearch-forward)
    ("C-c s r" . vr/query-repalce)
    ("C-c s R" . vr/replace)))

;; Interactive key descriptions
(use-package which-key
  :diminish (which-key-mode)
  :defer t
  :commands (which-key-enable-god-mode-support)
  :init
  (which-key-mode)
  :config
  (progn
    (setq which-key-idle-delay 0.4)
    (setq which-key-sort-order 'which-key-prefix-then-key-order)

    (setq which-key-key-replacement-alist
      '( ("<\\([[:alnum:]-]+\\)>" . "\\1")
         ("up"                    . "↑")
         ("right"                 . "→")
         ("down"                  . "↓")
         ("left"                  . "←")
         ("DEL"                   . "⌫")
         ("deletechar"            . "⌦")
         ("RET"                   . "⏎")))

    (setq which-key-description-replacement-alist
      '( ("Prefix Command" . "prefix")
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

    (which-key-enable-god-mode-support)))

;; Distraction-free editing
(use-package writeroom-mode
  :bind
  ("C-c t r" . writeroom-mode))

;; Trim trailing white-space
(use-package ws-butler
  :diminish (ws-butler-mode . " ☯")
  :commands (ws-butler-global-mode)
  :defer 1
  :config
  (ws-butler-global-mode))

;; Temporary one window
(use-package zoom-window
  :bind
  ("C-c w z" . zoom-window-zoom)
  :config
  (setq zoom-window-mode-line-color "PaleGoldenrod"))

;;; Language packages

;; Support for Bats
(use-package bats-mode
  :mode ("\\.bats\\'" . bats-mode))

;; Support for CSS
(use-package css-mode
  :mode ("\\.css\\'" . css-mode))

;; Support for CSV
(use-package csv-mode
  :mode ("\\.csv\\'" . csv-mode))

;; Support for Dockerfiles
(use-package dockerfile-mode
  :mode ("Dockerfile.*\\'" . dockerfile-mode))

;; Support for Elixir
(use-package elixir-mode
  :mode ( ("\\.ex\\'"      . elixir-mode)
          ("\\.exs\\'"     . elixir-mode)
          ("mix\\.lock\\'" . elixir-mode))
  :config
  (progn
    (use-package alchemist
      :diminish (alchemist-mode)
      :init
      (add-hook 'elixir-mode-hook #'alchemist-mode))

    (use-package flycheck-credo
      :config
      (flycheck-credo-setup))))

;; EPUB Reader
(use-package ereader
  :if window-system
  :mode ("\\.epub$" . ereader-mode)
  :init
  (add-hook 'ereader-mode-hook #'my-epub-mode-hook)
  :preface
  (defun my-epub-mode-hook ()
    "Setup Epub mode."
    (page-break-lines-mode +1)))

;; Support for Erlang
(use-package erlang
  :mode ( ("\\.erl\\'" . erlang-mode)
          ("\\.hrl\\'" . erlang-mode)
          ("\\.xrl\\'" . erlang-mode))
  :config
  (use-package distel
    :defer t
    :ensure nil
    :load-path "/usr/share/distel/elisp/"
    :commands (erlang-extended-mode)
    :init
    (add-hook 'erlang-mode-hook #'erlang-extended-mode)
    :config
    (progn
      (defvar inferior-erlang-prompt-timeout t)
      ;; default node name to emacs@localhost
      (setq inferior-erlang-machine-options '("-sname" "emacs"))
      (setq erl-nodename-cache
        (make-symbol
          (concat
            "emacs@"
            (car (split-string (shell-command-to-string "hostname"))))))))
  (use-package company-distel
    :after distel
    :preface
    (progn
      (autoload 'company-mode "company")
      (defun my-erlang-company-setup ()
        (setq-local company-backends '(company-distel))
        (company-mode)))
    :config
    (add-hook 'erlang-mode-hook #'my-erlang-company-setup)))

;; Support for Fish
(use-package fish-mode
  :mode ( ("\\.fish\\'"           . fish-mode)
          ("/fish_funced\\..*\\'" . fish-mode))
  :commands (fish-mode)
  :preface
  (defun my-fish-mode-hook ()
    "Setup Fish mode."
    (add-hook 'before-save-hook #'fish_indent-before-save))
  :config
  (add-hook 'fish-mode-hook #'my-fish-mode-hook))

(use-package gitattributes-mode :defer t)
(use-package gitconfig-mode     :defer t)
(use-package gitignore-mode     :defer t)

;; Support for Go
(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :functions (godef-jump)
  :bind
  ( :map go-mode-map
    ("M-." . godef-jump)
    ("M-*" . pop-tag-mark))
  :preface
  (defun my-go-mode-hook ()
    "Setup Go mode."
    ;; Use goimports instead of gofmt
    (setq gofmt-command "goimports")

    ;; Run gofmt before save
    (add-hook 'before-save-hook #'gofmt-before-save)

    ;; Customize compile command to run go build
    (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
        "go build -v && go test -v && go vet")))
  :config
  (progn
    (evil-define-key 'normal go-mode-map (kbd "K") #'godoc-at-point)

    (add-hook 'go-mode-hook #'my-go-mode-hook)

    ;; Completion support
    (use-package company-go
      :after go-mode
      :preface
      (progn
        (autoload 'company-mode "company")
        (defun my-go-company-setup ()
          (setq-local company-backends '(company-go))
          (company-mode)))
      :config
      (progn
        (setq company-go-show-annotation t)
        (add-hook 'go-mode-hook #'my-go-company-setup)))

    ;; Documentation support
    (use-package go-eldoc
      :after go-mode
      :init
      (add-hook 'go-mode-hook #'go-eldoc-setup))

    ;; Ask questions about Go source code
    (use-package go-guru
      :bind
      ( :map go-mode-map
        ("C-c f d" . go-guru-definition)
        ("C-c f r" . go-guru-referrers)
        ("C-c f s" . go-guru-callstack)
        ("C-c f i" . go-guru-implements))
      :init
      (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode))

    ;; Check for unchecked errors
    (use-package go-errcheck
      :after go-mode
      :commands (go-errcheck))

    ;; Test runner
    (use-package gotest)))

;; Support for Haskell
(use-package haskell-mode
  :diminish interactive-haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :bind
  ( :map haskell-mode-map
    ("C-c c c" . haskell-compile)
    ("<f5>"    . haskell-compile))
  :init
  (remove-hook 'haskell-mode-hook 'interactive-haskell-mode)
  :config
  (progn
    ;; Use interpreter "stack ghci"
    (setq haskell-process-type 'stack-ghci)

    ;; Fancy symbols
    (setq haskell-font-lock-symbols t)

    (setq haskell-interactive-mode-eval-mode 'haskell-mode)
    (setq haskell-interactive-mode-include-file-name nil)
    (setq haskell-notify-p t)
    (setq haskell-process-auto-import-loaded-modules t)
    (setq haskell-process-show-debug-tips nil)
    (setq haskell-process-suggest-haskell-docs-imports t)
    (setq haskell-process-suggest-hoogle-imports nil)
    (setq haskell-process-suggest-remove-import-lines t)
    (setq haskell-process-use-presentation-mode t)
    (setq haskell-stylish-on-save t)
    (setq haskell-tags-on-save nil)

    ;; Smart indentation
    (use-package hi2
      :diminish (hi2-mode . " ⇥")
      :defer t
      :commands (turn-on-hi2 hi2-mode)
      :init
      (add-hook 'haskell-mode-hook #'turn-on-hi2)
      :config
      ;; Unbind RET binding since the shm mode binding is preferred.
      (eval-after-load "hi2"
        '(define-key hi2-mode-map [?\r] nil))

      (add-to-list 'editorconfig-indentation-alist
        '(haskell-mode hi2-layout-offset hi2-left-offset hi2-ifte-offset))

      (add-hook 'editorconfig-custom-hooks
        (lambda (props)
          "Use half indentation space for keyword `where'."
          (let ((indent_size (gethash 'indent_size props)))
            (setq-default hi2-where-pre-offset
              (/ (string-to-number (if indent_size indent_size "4"))
                2))
            (setq-default hi2-where-post-offset
              (/ (string-to-number (if indent_size indent_size "4"))
                2))))))

    ;; Interactive development for Haskell
    ;; (completions, type checking, jump to definition, type selection,
    ;; suggestions)
    (use-package intero
      :diminish (intero-mode . " λ")
      :defer t
      :commands (intero-mode)
      :preface
      (progn
        (autoload 'company-mode "company")
        (defun my-haskell-company-setup ()
          (setq-local company-backends '(company-intero))
          (company-mode)))
      :init
      (progn
        (add-hook 'haskell-mode-hook #'intero-mode)
        (add-hook 'haskell-mode-hook #'my-haskell-company-setup))
      :config
      (setq haskell-process-args-stack-ghci
        '("--ghc-options=-ferror-spans" "--with-ghc=intero")))

    ;; Structured editing operations
    (use-package shm
      :defer t
      :commands ( structured-haskell-mode structured-haskell-repl-mode
                  shm/newline-indent evil-insert-state)
      :functions (evil-maybe-remove-spaces)
      :preface
      (progn
        (defun haskell-shm-setup ()
          "Setup SHM mode."
          (structured-haskell-mode)
          (hl-line-mode -1)
          (haskell-indentation-mode -1))

        (autoload 'evil-define-key "evil-core")
        (autoload 'evil-insert-state "evil-core")
        (autoload 'evil-maybe-remove-spaces "evil-core")

        (defun evil-shm/open-above (count)
          "Insert a new line above point and switch to Insert state.
The insertion will be repeated COUNT times."
          (interactive "p")
          (back-to-indentation)
          (save-excursion (shm/newline-indent))
          (setq evil-insert-count count
            evil-insert-lines t
            evil-insert-vcount nil)
          (evil-insert-state 1)
          (add-hook 'post-command-hook #'evil-maybe-remove-spaces))

        (defun evil-shm/open-below (count)
          "Insert a new line below point and switch to Insert state.
The insertion will be repeated COUNT times."
          (interactive "p")
          (goto-char (line-end-position))
          (shm/newline-indent)
          (setq evil-insert-count count
            evil-insert-lines t
            evil-insert-vcount nil)
          (evil-insert-state 1)
          (add-hook 'post-command-hook #'evil-maybe-remove-spaces)))
      :init
      (progn
        (add-hook 'haskell-mode-hook #'haskell-shm-setup)
        (add-hook 'haskell-interactive-mode-hook #'structured-haskell-repl-mode))
      :config
      (progn
        (setq shm-auto-insert-bangs t)
        (setq shm-auto-insert-skeletons t)
        (setq shm-indent-point-after-adding-where-clause t)
        (setq shm-use-hdevtools t)
        (setq shm-use-presentation-mode t)

        (evil-define-key 'normal shm-map
          (kbd "O") #'evil-shm/open-above
          (kbd "o") #'evil-shm/open-below)

        (set-face-background 'shm-current-face (face-attribute 'hl-line :background))
        (set-face-background 'shm-quarantine-face "#fff0f0")))))


;; Suppor for JavaScript
(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (progn
    ;; JavaScript code analyzer
    (use-package tern
      :defer t
      :commands (tern-mode)
      :init
      (add-hook 'js2-mode-hook #'tern-mode))

    ;; Completion for Tern
    (use-package company-tern
      :after tern
      :commands (company-tern)
      :preface
      (progn
        (autoload 'company-mode "company")
        (defun my-js-company-setup ()
          (setq-local company-backends '(company-tern))
          (company-mode)))
      :config
      (add-hook 'tern-mode-hook #'my-js-company-setup))))

;; Support for JSON
(use-package json-mode
  :mode ("\\.json\\'" . json-mode))

;; Support for Markdown
(use-package markdown-mode
  :mode
  ( ("\\`README\\.md\\'" . gfm-mode)
    ("\\.md\\'"          . markdown-mode)
    ("\\.markdown\\'"    . markdown-mode))
  :commands (markdown-mode gfm-mode)
  :bind
  ( :map markdown-mode-map
    ("TAB"             . nil)
    ("<S-iso-lefttab>" . nil)
    ("<S-tab>"         . nil)
    ("<backtab>"       . nil))
  :preface
  (defun my-markdown-setup ()
    "Setup Markdown mode."
    (variable-pitch-mode t))
  :config
  (progn
    (setq markdown-enable-wiki-links t)
    (setq markdown-wiki-link-fontify-missing t)

    (add-hook 'markdown-mode-hook #'my-markdown-setup)
    (add-hook 'gfm-mode-hook #'turn-off-auto-fill)

    ;; Typography
    (set-face-attribute 'markdown-pre-face nil :inherit 'fixed-pitch)
    (set-face-attribute 'markdown-inline-code-face nil :inherit 'fixed-pitch)

    (set-face-attribute 'markdown-header-face-1 nil :height 1.8)
    (set-face-attribute 'markdown-header-face-2 nil :height 1.4)
    (set-face-attribute 'markdown-header-face-3 nil :height 1.2)
    (set-face-attribute 'markdown-header-face-4 nil :height 1.0)
    (set-face-attribute 'markdown-header-face-5 nil :height 1.0)
    (set-face-attribute 'markdown-header-face-6 nil :height 1.0)))

;; Support for PDF
(use-package pdf-tools
  :if window-system
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :commands (pdf-tools-install))

;; Support for PKGBUILD
(use-package pkgbuild-mode
  :mode ("/PKGBUILD\\'" . pkgbuild-mode))

;; Support for PlantUML
(use-package plantuml-mode
  :mode ("\\.p\\(lant\\)?uml\\'" . plantuml-mode)
  :config
  (progn
    (setq plantuml-java-command "java-headless")
    (setq plantuml-jar-path "/opt/plantuml/plantuml.jar")))

;; Support for Protocol Buffers
(use-package protobuf-mode
  :mode ("\\.proto\\'" . protobuf-mode))

;; Support for Puppet
(use-package puppet-mode
  :mode
  ( ("\\.pp\\'"      . puppet-mode)
    ("Puppetfile\\'" . puppet-mode)))

;; Support for Python
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :preface
  (defun my-python-setup ()
    "Setup Python mode."
    (highlight-indent-guides-mode))
  :config
  (progn
    (add-hook 'python-mode-hook #'my-python-setup)

    ;; Code navigation, documentation lookup and completion
    (use-package anaconda-mode
      :defer t
      :commands (anaconda-mode anaconda-eldoc-mode)
      :init
      (progn
        (add-hook 'python-mode-hook 'anaconda-mode)
        (add-hook 'python-mode-hook 'anaconda-eldoc-mode))
      :config
      (progn
        (setq anaconda-mode-installation-directory
          (concat my-data-directory "anaconda-mode"))))

    ;; Completion for anaconda mode
    (use-package company-anaconda
      :after anaconda-mode
      :preface
      (progn
        (autoload 'company-mode "company")
        (defun my-python-company-setup ()
          (setq-local company-backends '(company-anaconda))
          (company-mode)))
      :init
      (add-hook 'python-mode-hook #'my-python-company-setup))))

;; Support for ReStructured Text
(use-package rst
  :mode
  ( ("\\.txt\\'"  . rst-mode)
    ("\\.rst\\'"  . rst-mode)
    ("\\.rest\\'" . rst-mode))
  :bind
  ( :map rst-mode-map
    ("M-RET" . rst-insert-list))
  :preface
  (defun my-rst-setup ()
    "Setup ReStructured Text mode."
    (variable-pitch-mode t))
  :config
  (progn
    (add-hook 'rst-mode-hook #'my-rst-setup)

    ;; Header underline display
    (set-face-attribute 'rst-adornment nil
      :strike-through "black"
      :foreground (face-attribute 'default :background)
      :background (face-attribute 'default :background))

    ;; Typography
    (set-face-attribute 'rst-literal nil :inherit 'fixed-pitch)

    (set-face-attribute 'rst-level-1 nil
      :background nil :height 1.8 :weight 'bold)
    (set-face-attribute 'rst-level-2 nil
      :background nil :height 1.4 :weight 'bold)
    (set-face-attribute 'rst-level-3 nil
      :background nil :height 1.2 :weight 'bold)
    (set-face-attribute 'rst-level-4 nil
      :background nil :height 1.0 :weight 'bold)
    (set-face-attribute 'rst-level-5 nil
      :background nil :height 1.0 :weight 'bold)
    (set-face-attribute 'rst-level-6 nil
      :background nil :height 1.0 :weight 'bold)))

;; Support for Ruby
(use-package enh-ruby-mode
  :mode
  ( ("\\.rb\\'"       . enh-ruby-mode)
    ("\\.ru\\'"       . enh-ruby-mode)
    ("\\.rake\\'"     . enh-ruby-mode)
    ("\\.thor\\'"     . enh-ruby-mode)
    ("\\.gemspec\\'"  . enh-ruby-mode)
    ("Gemfile\\'"     . enh-ruby-mode)
    ("Rakefile\\'"    . enh-ruby-mode)
    ("Vagrantfile\\'" . enh-ruby-mode))
  :interpreter ("pry" . enh-ruby-mode)
  :config
  (progn
    ;; Code navigation, documentation lookup and completion
    (use-package robe
      :defer t
      :commands (robe-mode robe-start)
      :preface
      (progn
        (autoload 'company-mode "company")
        (defun my-ruby-setup ()
          "Setup Ruby mode."
          (robe-mode)
          (setq-local company-backends '(company-robe))
          (company-mode)))
      :init
      (add-hook 'enh-ruby-mode-hook #'my-ruby-setup))

    ;; REPL buffer
    (use-package inf-ruby
      :defer t
      :commands (inf-ruby inf-ruby-auto-enter)
      :config
      (progn
        (setq inf-ruby-default-implementation "pry")
        (add-hook 'enh-ruby-mode-hook #'inf-ruby-minor-mode)
        (add-hook 'compilation-filter-hook #'inf-ruby-auto-enter)

        (use-package company-inf-ruby
          :preface
          (progn
            (autoload 'company-mode "company")
            (defun my-ruby-repl-company-setup ()
              (setq-local company-backends '(company-inf-ruby))
              (company-mode)))
          :config
          (add-hook 'enh-ruby-mode-hook #'my-ruby-repl-company-setup))))

    (use-package ruby-test-mode
      :commands (ruby-test-run-at-point ruby-test-run))

    (use-package rubocop
      :diminish (rubocop-mode)
      :commands (rubocop-mode)
      :config
      (add-hook 'enh-ruby-mode-hook #'rubocop-mode))))

;; Support for Rust
(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (progn
    (setq rust-format-on-save (executable-find "rustfmt"))

    ;; Flycheck support for Rust
    (use-package flycheck-rust
      :commands (flycheck-rust-setup)
      :init
      (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

    ;; Racer support (completion, definition lookup, describe function/type)
    (use-package racer
      :defer t
      :defines (racer-rust-src-path)
      :commands (racer-mode racer-describe racer-find-definition)
      :init
      (progn
        (add-hook 'racer-mode-hook #'eldoc-mode)
        (add-hook 'rust-mode-hook #'racer-mode))
      :config
      (progn
        (unless (getenv "RUST_SRC_PATH")
          (setenv "RUST_SRC_PATH" racer-rust-src-path))

        (with-eval-after-load 'rust-mode
          (evil-define-key 'normal rust-mode-map (kbd "K") #'racer-describe)
          (evil-define-key 'normal rust-mode-map (kbd "M-.") #'racer-find-definition))))

    ;; Perform cargo tasks within Rust projects
    (use-package cargo
      :defer t
      :config
      (add-hook 'rust-mode-hook #'cargo-minor-mode))))


;; Support for POSIX-based shell scripts
(use-package sh-script
  :mode
  ( ("\\.sh\\'"   . sh-mode)
    ("\\.zsh\\'"  . sh-mode)
    ("\\.bash\\'" . sh-mode))
  :config
  (progn
    ;; Use regular indentation for line-continuation
    (setq sh-indent-after-continuation 'always)

    ;; Typography
    (set-face-attribute 'sh-quoted-exec nil
      :background (face-attribute 'font-lock-builtin-face :background)
      :foreground (face-attribute 'font-lock-builtin-face :foreground))))

;; Support for slim templates
(use-package slim-mode
  :mode ("\\.slim\\'" . slim-mode))

;; Support for SQL
(use-package sql
  :mode ("\\.sql\\'" . sql-mode)
  :commands (sql-connect sql-set-product)
  :bind
  ( ("C-c a s" . sql-connect)
    :map sql-mode-map
    ("C-c m p" . sql-set-product)))

;; Support for systemd files
(use-package systemd
  :defer t
  :config
  (add-hook 'systemd-mode-hook
    (lambda () (run-hooks 'prog-mode-hook))))

;; Support for thrift files
(use-package thrift
  :mode ("\\.thrift\\'" . thrift-mode)
  :config
  (add-hook 'thrift-mode-hook
    (lambda () (run-hooks 'prog-mode-hook))))

;; Support for web-related files
(use-package web-mode
  :mode
  ( ("\\.html?\\'"      . web-mode)
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
  (progn
    (setq web-mode-enable-current-element-highlight t)

    ;; No padding for nested sections inside HTML
    (add-hook 'editorconfig-custom-hooks
      (lambda (props)
        (setq web-mode-block-padding 0)
        (setq web-mode-script-padding 0)
        (setq web-mode-style-padding 0)))

    ;; Zen Coding support
    (use-package emmet-mode
      :defer t
      :commands (emmet-mode)
      :init
      (add-hook 'web-mode-hook #'emmet-mode))

    ;; Completion for web mode
    (use-package company-web
      :preface
      (progn
        (autoload 'company-mode "company")
        (defun my-web-company-setup ()
          (setq-local company-backends '(company-web-html))
          (company-mode)))
      :config
      (add-hook 'web-mode-hook #'my-web-company-setup))))

;; Support for YAML files
(use-package yaml-mode
  :mode ("\\.\\(e?ya?\\|ra\\)ml\\'" . yaml-mode)
  :config
  ;; Documentation lookup for Ansible,
  ;; this can be issued by `C-c ?'
  (use-package ansible-doc
    :diminish (ansible-doc-mode)
    :commands (ansible-doc-mode)
    :config
    (add-hook 'yaml-mode-hook #'ansible-doc-mode))

  ;; Completion for Ansible keywords
  (use-package company-ansible
    :preface
    (progn
      (autoload 'company-mode "company")
      (defun my-yaml-company-setup ()
        (setq-local company-backends '(company-ansible))
        (company-mode)))
    :config
    (add-hook 'yaml-mode-hook #'my-yaml-company-setup)))

;; Completion for shell functions and executable files in PATH
(use-package company-shell
  :after sh-script
  :preface
  (progn
    (autoload 'company-mode "company")
    (defun my-sh-company-setup ()
      (setq-local company-backends '(company-shell))
      (company-mode))
    (defun my-fish-company-setup ()
      (setq-local company-backends '(company-fish-shell))
      (company-mode)))
  :config
  (progn
    (setq company-shell-delete-duplicates t)
    (add-hook 'sh-mode-hook #'my-sh-company-setup)
    (add-hook 'fish-mode-hook #'my-fish-company-setup)))

;;; Modeline
(setq-default mode-line-format
  '( "%e"
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

;;; Finish

;; Log startup time when interactive
(unless noninteractive
  (let ((elapsed
          (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
    `(lambda ()
       (let ((elapsed
               (float-time (time-subtract (current-time) emacs-start-time))))
         (message "Loading %s...done (%.3fs) [after-init]"
           ,load-file-name elapsed)))
    t))

;; Reset debug mode
(setq debug-on-error nil)
(setq debug-on-quit nil)

;; Load dynamic customization
(setq custom-file (concat my-data-directory "custom.el"))
(load custom-file)
;;; init.el ends here
