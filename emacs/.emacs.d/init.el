;;; init.el --- main configuration entry point

;;; Commentary:

;; Emacs configuration of Terje Larsen.

;;; Code:
;;; -*- lexical-binding: t -*-

;; Delay garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold (* 100 1024 1024))))

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
(setq initial-major-mode 'rst-mode)

;;; Appearance
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(use-package flatui-theme)
(use-package leuven-theme :defer t)
(use-package color-theme-sanityinc-tomorrow :defer t)
(use-package twilight-bright-theme :defer t)

(set-face-attribute 'default nil
                    :family "Input Mono Narrow"
                    :weight 'normal
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

;; Auto-executable scripts
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

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
        (cond ((eq tab-width 8) 2)
              ((eq tab-width 2) 4)
              (t 8)))
  (redraw-display)
  (message "Set tab-width to %d." tab-width))

(defun toggle-indent-mode-setting ()
  "Toggle indenting modes between tabs and spaces."
  (interactive)
  (setq indent-tabs-mode(if (eq indent-tabs-mode t) nil t))
  (message "Indenting using %s."
           (if (eq indent-tabs-mode t)
               "tabs"
             "spaces")))

(bind-key "C-=" 'toggle-tab-width-setting)
(bind-key "C-+" 'toggle-indent-mode-setting)

;; C-c
(bind-key "C-c TAB" 'ff-find-other-file) ; Open alternate file

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

;; Align text
(use-package align
  :commands (align)
  :bind
  (("C-c ["     . align-regexp)
   ("C-c x a a" . align)
   ("C-c x a c" . align-current)))

;; Auto-revert buffers for changed files
(use-package autorevert
  :diminish (auto-revert-mode . " ⎌")
  :defer t
  :init (global-auto-revert-mode)
  :config (setq auto-revert-verbose nil))

;; Bookmark locations
(use-package bookmark
  :bind ("C-c f b" . list-bookmarks)
  :config
  (progn
    (setq bookmark-save-flag t)
    (setq bookmark-default-file (concat my-cache-directory "bookmarks"))))

;; Turn bug references into buttons
(use-package bug-reference
  :defer t
  :init
  (progn
    (add-hook 'text-mode-hook #'bug-reference-mode)
    (add-hook 'magit-log-mode-hook #'bug-reference-mode)
    (add-hook 'prog-mode-hook #'bug-reference-prog-mode)))

;; Calendar view
(use-package calendar
  :bind ("C-c a c" . calendar)
  :config (setq calendar-week-start-day 1))

;; Compilation system
(use-package compile
  :functions (ansi-color-apply-on-region)
  :bind ("C-c c C" . recompile)
  :config
  (progn
    (setq compilation-always-kill t)
    (setq compilation-ask-about-save nil)
    (setq compilation-context-lines 3)
    (setq compilation-disable-input t)

    ;; Automatic scroll to first error
    (setq compilation-scroll-output 'first-error)
    ;; Skip warnings and info messages
    (setq compilation-skip-threshold 2)

    ;; Switch to compilation buffer after compile
    (add-hook 'compilation-finish-functions
              (lambda (buf str)
                (switch-to-buffer-other-window "*compilation*")
                (read-only-mode)
                (goto-char (point-max))
                ;; Allow closing buffer with q
                (local-set-key (kbd "q")
                               (lambda () (interactive)
                                 (quit-restore-window)))))

    ;; Filter ANSI escape codes in compilation-mode output
    (require 'ansi-color)
    (add-hook 'compilation-filter-hook
              (lambda ()
                (let ((inhibit-read-only t))
                  (ansi-color-apply-on-region compilation-filter-start
                                              (point)))))))

;; Delete selection upon insert
(use-package delsel
  :defer t
  :init (delete-selection-mode))

;; Diff tool
(use-package ediff
  :commands (ediff-copy-diff ediff-get-region-contents winner-undo)
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
  :preface
  ;; TODO: Check if smerge-keep-all can be sufficient instead.
  (defun ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff
     ediff-current-difference nil 'C nil
     (concat
      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

  (defun add-mappings-to-ediff-mode-map ()
    (define-key ediff-mode-map "d" 'ediff-copy-both-to-C)
    (define-key ediff-mode-map "j" 'ediff-next-difference)
    (define-key ediff-mode-map "k" 'ediff-previous-difference))
  :init
  (progn
    (add-hook 'ediff-quit-hook #'winner-undo)

    ;; Setting up the mappings through the bind command will leave them
    ;; behind, breaking all further modes. Setup with a hook instead.
    (add-hook 'ediff-keymap-setup-hook #'add-mappings-to-ediff-mode-map))
  :config
  (progn
    (setq ediff-window-setup-function 'ediff-setup-windows-plain)
    (setq ediff-split-window-function 'split-window-horizontally)
    (setq ediff-merge-split-window-function 'split-window-horizontally)))

;; Documentation in minibuffer
(use-package eldoc
  :diminish (eldoc-mode . " ℹ")
  :defer t
  :init
  (dolist (hook '(emacs-lisp-mode-hook
                  eval-expression-minibuffer-setup-hook
                  lisp-interaction-mode-hook))
    (add-hook hook #'eldoc-mode)))

;; Clever input
(use-package electric
  :defer t
  :init
  (progn
    ;; Indent line after RET
    (electric-indent-mode)
    ;; Auto-insert matching delimiters
    (electric-pair-mode)))

;; Tag-file handling
(use-package etags
  :bind
  (("M-T"     . tags-search)
   ("C-c s t" . tags-search))
  :config (setq tags-revert-without-query t))

;; Web browser
(use-package eww
  :commands (eww eww-browse-url)
  :bind
  (("C-c a w w" . eww)
   ("C-c a w u" . eww-browse-url))
  :defer t
  :init (add-hook 'eww-mode-hook 'buffer-face-mode))

;; Spell-checking
(use-package flyspell
  :diminish (flyspell-mode . " ≁")
  :bind ("C-c t s" . flyspell-mode)
  :defer t
  :init
  (progn
    (add-hook 'message-mode-hook 'flyspell-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)
    (add-hook 'text-mode-hook 'flyspell-mode))
  :config
  (progn
    (setq flyspell-issue-message-flag nil)
    (setq flyspell-issue-welcome-flag nil)))

;; Make links clickable
(use-package goto-addr
  :bind
  (("C-c t a" . goto-address-mode)
   ("C-c t A" . goto-address-prog-mode))
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook 'goto-address-prog-mode)
    (add-hook 'text-mode-hook 'goto-address-mode)))

;; Code folding
(use-package hideshow
  :diminish (hs-minor-mode)
  :defer t
  :init
  (add-hook 'prog-mode-hook #'hs-minor-mode))

;; Expansion and completion (of lines and blocks)
;; Can be used through the binding `M-/'
(use-package hippie-exp
  :bind ([remap dabbrev-expand] . hippie-expand)
  :config
  (setq hippie-expand-try-functions-list
        '(
          ;; Try to expand word "dynamically", searching the current buffer.
          try-expand-dabbrev
          ;; Try to expand word "dynamically", searching all other buffers.
          try-expand-dabbrev-all-buffers
          ;; Try to expand word "dynamically", searching the kill ring.
          try-expand-dabbrev-from-kill
          ;; Try to complete text as a file name, as many characters as unique.
          try-complete-file-name-partially
          ;; Try to complete text as a file name.
          try-complete-file-name
          ;; Try to expand word before point according to all abbrev tables.
          try-expand-all-abbrevs
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-list
          try-expand-line
          ;; Try to complete as an Emacs Lisp symbol, as many characters as unique.
          try-complete-lisp-symbol-partially
          ;; Try to complete word as an Emacs Lisp symbol.
          try-complete-lisp-symbol)))

;; Highlight current line
(use-package hl-line
  :defer t
  :init
  (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook 'hl-line-mode)))

;; Display images
(use-package image-file
  :defer t
  :init (auto-image-file-mode))

;; Track files recently opened
(use-package recentf
  :defer t
  :init
  (progn
    (defvar recentf-save-file (concat my-cache-directory "recentf"))
    (recentf-mode))
  :config
  (progn
    (setq recentf-auto-cleanup 300)
    (setq recentf-exclude
          (list "/\\.git/.*\\'" ; Git contents
                "/.cache/.*\\'" ; Cache files
                "/elpa/.*\\'"   ; Package files
                "/ssh:"         ; SSH files
                "/tmp/"         ; TMP files
                #'ignoramus-boring-p))
    (setq recentf-max-menu-items 64)
    (setq recentf-max-saved-items 512)))

;; Save mini buffer history
(use-package savehist
  :defer t
  :init (savehist-mode)
  :config
  (progn
    (setq history-delete-duplicates t)
    (setq history-length t)
    (setq savehist-file (concat my-cache-directory "history"))
    (setq savehist-save-minibuffer-history t)))

;; Remember point position in files
(use-package saveplace
  :defer t
  :init (setq-default save-place t)
  :config
  (progn
    (setq save-place-file (concat my-cache-directory "places"))
    (setq save-place-forget-unreadable-files nil)))

;; Recognize camel and snake case
;;
;; This changes the behavior of word-detection, as each part is now
;; detected as a separate word.
(use-package subword
  :diminish (subword-mode)
  :defer t
  :init (add-hook 'prog-mode-hook #'subword-mode))

;; Interface to OS shell
(use-package shell
  :bind ("C-c a t" . shell))

;; Terminal emulator
(use-package term
  :bind ("C-c a T" . ansi-term))

;; Show time
(use-package time
  :bind
  (("C-c a i" . emacs-init-time)
   ("C-c a C" . display-time-world)))

;; Display white-space characters
(use-package whitespace
  :diminish (whitespace-mode . " ␠")
  :bind ("C-c t w" . whitespace-mode)
  :defer t
  :config
  (progn
    (setq whitespace-line-column 100)
    (setq whitespace-style '(face spaces tabs trailing empty space-mark tab-mark))))

;; Undo and redo window configurations
(use-package winner
  :bind
  (("M-N" . winner-redo)
   ("M-P" . winner-undo))
  :init (winner-mode))

;;; Local packages

;; Support for RAML
(use-package raml-mode
  :ensure nil
  :load-path "vendor/raml-mode/"
  :defer t)

;; Show tab width configuration in mode-line
(use-package show-tab-width-mode
  :ensure nil
  :load-path "lisp/"
  :commands (show-tab-width-mode)
  :init (show-tab-width-mode))

;;; Packages

;; Fast window switching
(use-package ace-window
  :commands (ace-window)
  :bind
  (("C-c o"   . ace-window)
   ("C-c w w" . ace-window)))

;; Align wrapped lines
(use-package adaptive-wrap
  :commands (adaptive-wrap-prefix-mode)
  :defer t
  :init (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

(use-package aggressive-indent
  :diminish (aggressive-indent-mode)
  :commands (global-aggressive-indent-mode)
  :bind ("C-c t i" . aggressive-indent-mode)
  :defer 5
  :config
  (progn
    ;; Disabled modes
    (dolist (mode '(diff-auto-refine-mode dockerfile-mode))
      (add-to-list 'aggressive-indent-excluded-modes mode))

    ;; Disabled commands
    (dolist (command '(evil-undo-pop ws-butler-clean-region))
      (add-to-list 'aggressive-indent-protected-commands command))

    (global-aggressive-indent-mode +1)))

;; Position/matches count for search
(use-package anzu
  :diminish (anzu-mode)
  :bind
  (([remap query-replace]        . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)
   :map
   isearch-mode-map
   ([remap isearch-query-replace]        . anzu-isearch-query-replace)
   ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :init (global-anzu-mode)
  :config (use-package evil-anzu))

;; Automatically infer dictionary
(use-package auto-dictionary
  :bind
  (("C-c l l" . adict-change-dictionary)
   ("C-c l g" . adict-guess-dictionary)))

;; Use GitHub URL for bug reference
(use-package bug-reference-github
  :defer t
  :init (add-hook 'find-file-hook #'bug-reference-github-set-url-format))

;; Completion system
(use-package company
  :diminish (company-mode)
  :commands (global-company-mode)
  :functions (company-complete-common-or-cycle)
  :preface
  (defun company-indent-or-complete ()
    "Try to indent before trying to complete."
    (interactive)
    (if (looking-at "\\_>")
        (company-complete-common-or-cycle)
      (indent-according-to-mode)))
  :bind
  (:map
   company-mode-map
   ("<tab>" . company-indent-or-complete)
   :map
   company-active-map
   ("C-e"       . company-complete-selection)
   ("C-f"       . company-complete-selection)
   ("TAB"       . company-complete-common-or-cycle)
   ("<tab>"     . company-complete-common-or-cycle)
   ("S-TAB"     . company-select-previous)
   ("<backtab>" . company-select-previous)
   ("RET"       . nil)
   ("<return>"  . nil)
   ("<escape>"  . company-abort))
  :defer t
  :preface
  (progn
    (defvar-local company-whitespace-mode-on-p nil)

    (defun company-turn-off-whitespace (&rest ignore)
      (when (boundp 'whitespace-mode)
        (setq company-whitespace-mode-on-p whitespace-mode)
        (when whitespace-mode (whitespace-mode -1))))

    (defun company-maybe-turn-on-whitespace (&rest ignore)
      (when company-whitespace-mode-on-p (whitespace-mode 1))))
  :init
  (progn
    (add-hook 'company-completion-started-hook 'company-turn-off-whitespace)
    (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-whitespace)
    (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-whitespace)
    (global-company-mode))
  :config
  (progn
    (setq company-echo-delay 0)
    (setq company-idle-delay .3)
    (setq company-minimum-prefix-length 2)
    (setq company-require-match 'never)
    (setq company-selection-wrap-around t)
    (setq company-show-numbers t)
    (setq company-tooltip-align-annotations t)
    (setq company-tooltip-flip-when-above t)

    ;; Emoji-word completion
    (use-package company-emoji
      :config (add-to-list 'company-backends 'company-emoji))

    ;; Documentation popup
    (use-package company-quickhelp
      :defer t
      :init (company-quickhelp-mode)
      :config (use-package pos-tip))

    ;; History based sorting
    (use-package company-statistics
      :defer t
      :init
      (progn
        (setq company-statistics-file
              (concat my-cache-directory "company-statistics-cache.el"))
        (company-statistics-mode)))))

;; Text scale for all buffers
(use-package default-text-scale
  :bind
  (("C-M-=" . default-text-scale-increase)
   ("C-M--" . default-text-scale-decrease)
   ("<C-mouse-4>"                . text-scale-increase)
   ("<C-mouse-5>"                . text-scale-decrease)
   ("<left-margin> <C-mouse-4>"  . text-scale-increase)
   ("<left-margin> <C-mouse-5>"  . text-scale-decrease)
   ("<right-margin> <C-mouse-4>" . text-scale-increase)
   ("<right-margin> <C-mouse-5>" . text-scale-decrease)))

;; Notes managing
(use-package deft
  :commands (deft)
  :defer t
  :config
  (progn
    (setq deft-directory "~/notes")
    (setq deft-extensions '("txt" "tex" "org" "md" "rst"))))

;; Display change diff in fringe with colors
(use-package diff-hl
  :commands (global-diff-hl-mode diff-hl-dired-mode)
  :functions (diff-hl-magit-post-refresh)
  :defer t
  :init
  (progn
    (global-diff-hl-mode)
    (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

;; Dired enhancements
(use-package dired+
  :defer 1
  :init (diredp-toggle-find-file-reuse-dir 1)
  :config
  (progn
    (setq diredp-image-preview-in-tooltip 300)
    ;; Remove color and font decoration
    (setq font-lock-maximum-decoration '((dired-mode) (t . t)))))

;; Dired sidebar
(use-package dired-toggle
  :commands (dired-toggle)
  :bind ("C-c t d" . dired-toggle))

;; Copy and select things
(use-package easy-kill
  :bind
  (([remap kill-ring-save] . easy-kill)
   ([remap mark-sexp]      . easy-mark)))

;; Use editorconfig for indentation configuration
(use-package editorconfig
  :if (executable-find "editorconfig")
  :diminish (editorconfig-mode . " ⚙")
  :mode ("\\.editorconfig\\'" . conf-unix-mode)
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook #'editorconfig-mode)
    (add-hook 'text-mode-hook #'editorconfig-mode)))

;; VIM-behavior
(use-package evil
  :commands (evil-mode evil-normal-state evil-delay)
  :bind
  (:map
   evil-normal-state-map
   ("<tab>"     . next-buffer)
   ("<backtab>" . previous-buffer))
  :defer t
  :init (evil-mode)
  :config
  (progn
    (setq evil-auto-indent t)
    (setq evil-want-fine-undo 'fine)

    ;; evil-want-Y-yank-to-eol must be set via customize to have an effect
    (customize-set-variable 'evil-want-Y-yank-to-eol t)

    ;; Insert state uses Emacs key-map.
    (setq evil-insert-state-map (make-sparse-keymap))
    (define-key evil-insert-state-map (kbd "<escape>") #'evil-normal-state)

    ;; Map SPC to C-c in non-insert modes
    (define-key evil-normal-state-map (kbd "SPC") (simulate-key-press "C-c"))
    (define-key evil-visual-state-map (kbd "SPC") (simulate-key-press "C-c"))

    (use-package evil-matchit
      :init (global-evil-matchit-mode))

    (use-package evil-surround
      :init (global-evil-surround-mode))))

;; Highlight TODO inside comments and strings
(use-package fic-mode
  :init (add-hook 'prog-mode-hook #'fic-mode)
  :config
  (dolist (face '(fic-face fic-author-face))
    (set-face-background face (face-attribute 'font-lock-string-face :background))
    (set-face-foreground face (face-attribute 'font-lock-string-face :foreground))))

;; Linting and syntax checking
(use-package flycheck
  :commands (flycheck)
  :functions (flycheck-display-error-messages-unless-error-list)
  :bind
  (("C-c t f" . flycheck-mode)
   ("M-n"     . flycheck-next-error)
   ("M-p"     . flycheck-previous-error))
  :defer 5
  :init (global-flycheck-mode)
  :config
  (progn
    (setq-default flycheck-emacs-lisp-load-path 'inherit)
    (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
    (setq flycheck-standard-error-navigation nil)

    ;; Popup for errors
    (use-package flycheck-pos-tip
      :after flycheck
      :commands (flycheck-pos-tip-mode)
      :config (flycheck-pos-tip-mode))

    ;; Helm navigation
    (use-package helm-flycheck)))

;; Git commit popup
(use-package git-messenger
  :bind ("C-c g m" . git-messenger:popup-message))

;; Step through git history of file
(use-package git-timemachine
  :bind ("C-c g t" . git-timemachine-toggle))

;; Open file on GitHub
(use-package github-browse-file
  :commands (github-browse-file github-browse-file-blame)
  :bind
  (("C-c g h s" . github-browse-file)
   ("C-c g h b" . github-browse-file-blame)))

;; Ctrl prefix everything
(use-package god-mode
  :bind ("C-c SPC" . god-local-mode)
  :init
  (use-package evil-god-state
    :commands (evil-execute-in-god-state)
    :bind
    (:map
     evil-normal-state-map
     ("C-c SPC" . evil-execute-in-god-state))
    :config (evil-define-key 'god global-map [escape] 'evil-god-state-bail)))

;; Completion system
(use-package helm
  :diminish (helm-mode)
  :commands (helm-mode helm-autoresize-mode)
  :bind (([remap execute-extended-command] . helm-M-x)
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
    (defvar helm-M-x-fuzzy-match t)
    (defvar helm-buffers-fuzzy-matching t)
    (defvar helm-display-header-line nil)
    (defvar helm-ff-file-name-history-use-recentf t)
    (defvar helm-ff-search-library-in-sexp t)
    (defvar helm-imenu-execute-action-at-once-if-one nil)
    (defvar helm-imenu-fuzzy-match t)
    (defvar helm-recentf-fuzzy-match t)

    ;; Describe key bindings
    (use-package helm-descbinds
      :bind ("C-h b" . helm-descbinds)
      :init (fset 'describe-bindings 'helm-descbinds))

    ;; Run make tasks
    (use-package helm-make
      :bind
      (("C-c c c" . helm-make-projectile)
       ("<f5>"    . helm-make-projectile)))

    ;; Squeezed line navigation
    (use-package helm-swoop
      :bind
      (("C-c s s"   . helm-swoop)
       ("C-c s S"   . helm-multi-swoop)
       ("C-c s C-s" . helm-multi-swoop-all))
      :config (setq helm-swoop-split-window-function 'helm-default-display-buffer))

    ;; Systemd control
    (use-package helm-systemd
      :bind ("C-c a d" . helm-systemd))))

;; Indentation guides
(use-package highlight-indent-guides
  :init (add-hook 'emacs-lisp-mode-hook 'highlight-indent-guides-mode)
  :config
  (progn
    (setq highlight-indent-guides-method 'character)
    (set-face-foreground 'highlight-indent-guides-character-face
                         (face-attribute 'fringe :background))))

;; Highlight current line number
(use-package hlinum
  :defer t
  :init (hlinum-activate)
  :config
  (progn
    (set-face-foreground 'linum-highlight-face
                         "#AF0000")
    (set-face-background 'linum-highlight-face
                         (face-attribute 'linum :background))))

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
  (("C-c g c" . magit-clone)
   ("C-c g s" . magit-status)
   ("C-c g b" . magit-blame)
   ("C-c g l" . magit-log-buffer-file)
   ("C-c g p" . magit-pull))
  :init (global-magit-file-mode)
  :config
  (progn
    (setenv "GIT_PAGER" "")

    (setq magit-log-buffer-file-locked t)
    (setq magit-refs-show-commit-count 'all)
    (setq magit-save-repository-buffers 'dontask)

    (use-package evil-magit)
    (use-package magit-gh-pulls
      :init (add-hook 'magit-mode-hook #'turn-on-magit-gh-pulls))))

;; Mode icons
(use-package mode-icons
  :init (mode-icons-mode))

;; REST REPL
(use-package restclient
  :defer t
  :config
  (use-package company-restclient
    :preface
    (progn
      (autoload 'company-mode "company")
      (defun my-restclient-company-hook ()
        (setq-local company-backends '(company-restclient))
        (company-mode)))
    :config (add-hook 'restclient-mode-hook #'my-restclient-company-hook)))

;; Display page breaks as a horizontal line
(use-package page-break-lines
  :diminish (page-break-lines-mode)
  :defer t
  :init (global-page-break-lines-mode))

;; Workspaces with buffer isolation
(use-package persp-mode
  :commands
  (persp-mode
   persp-switch persp-prev persp-next
   def-auto-persp
   persp-auto-persps-pickup-buffers
   projectile-project-name projectile-project-p)
  :bind
  (("C-c RET" . persp-switch)
   ("M-["     . persp-prev)
   ("M-]"     . persp-next))
  :init
  (progn
    (setq persp-save-dir (concat my-cache-directory "persp-confs/"))
    (persp-mode))
  :config
  (progn
    ;; Automatically add all free buffers to the current perspective.
    (setq persp-add-buffer-on-after-change-major-mode 'free)
    (setq persp-add-buffer-on-find-file 'if-not-autopersp)
    (setq persp-autokill-buffer-on-remove 'kill-weak)
    ;; Prevent loading of existing perspectives when opening new frames.
    (setq persp-emacsclient-init-frame-behaviour-override nil)

    (add-hook 'persp-after-load-state-functions
              (lambda (&rest args) (persp-auto-persps-pickup-buffers)) t)

    (set-face-background 'persp-face-lighter-buffer-not-in-persp
                         (face-attribute 'isearch-fail :background))
    (set-face-foreground 'persp-face-lighter-buffer-not-in-persp
                         (face-attribute 'isearch-fail :foreground))

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
      (lambda () (projectile-project-name)))))

;; Project interaction and navigation
(use-package projectile
  :diminish (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map)
  :defer 5
  :init
  (progn
    (setq projectile-cache-file
          (concat my-cache-directory "projectile.cache"))
    (setq projectile-known-projects-file
          (concat my-cache-directory "projectile-bookmarks.eld"))
    (projectile-mode))
  :config
  (progn
    (add-to-list 'projectile-globally-ignored-directories ".cache")
    (add-to-list 'projectile-globally-ignored-directories "node_modules")
    (add-to-list 'projectile-globally-ignored-directories "tmp")
    (add-to-list 'projectile-globally-ignored-directories "vendor")

    ;; Completion system
    (use-package helm-projectile
      :bind ("C-c s g" . helm-projectile-grep)
      :init (helm-projectile-on)
      :config
      (progn
        (setq helm-projectile-fuzzy-match t)
        (setq projectile-completion-system 'helm)))))

;; Striped directory listing
(use-package stripe-buffer
  :defer 1
  :init (add-hook 'dired-mode-hook 'stripe-listify-buffer)
  :config (set-face-background 'stripe-highlight
                               (face-attribute 'fringe :background)))

;; Utilities for opening files with sudo
(use-package sudo-edit
  :defer t
  :bind ("C-c f s" . sudo-edit))

;; Save buffers when focus is lost
(use-package super-save
  :diminish (super-save-mode)
  :init (super-save-mode))

;; Tree based undo history
(use-package undo-tree
  :diminish (undo-tree-mode)
  :bind ("C-c u" . undo-tree-visualize)
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
  (([remap isearch-backward] . vr/isearch-backward)
   ([remap isearch-forward]  . vr/isearch-forward)
   ("C-c s r" . vr/query-repalce)
   ("C-c s R" . vr/replace)))

;; Interactive key descriptions
(use-package which-key
  :diminish (which-key-mode)
  :commands (which-key-enable-god-mode-support)
  :defer t
  :init (which-key-mode)
  :config
  (progn
    (setq which-key-idle-delay 0.4)
    (setq which-key-sort-order 'which-key-prefix-then-key-order)

    (push '(("<\\([[:alnum:]-]+\\)>" . nil) . ("\\1" . nil))
          which-key-replacement-alist)
    (push '(("\\`\\?\\?\\'" . nil) . ("λ" . nil))
          which-key-replacement-alist)

    (push '(("<up>" . nil) . ("↑" . nil)) which-key-replacement-alist)
    (push '(("<right>" . nil) . ("→" . nil)) which-key-replacement-alist)
    (push '(("<down>" . nil) . ("↓" . nil)) which-key-replacement-alist)
    (push '(("<left>" . nil) . ("←" . nil)) which-key-replacement-alist)

    (add-to-list 'which-key-replacement-alist '(("SPC" . nil) . ("␣" . nil)))
    (add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("↹" . nil)))
    (add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("⏎" . nil)))
    (add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("⌫" . nil)))
    (add-to-list 'which-key-replacement-alist '(("deletechar" . nil) . ("⌦" . nil)))

    (add-to-list 'which-key-replacement-alist
                 '((nil . "projectile-") . (nil . "pt-")))
    (add-to-list 'which-key-replacement-alist
                 '((nil . "helm-") . (nil . "h-")))
    (add-to-list 'which-key-replacement-alist
                 '((nil . "flycheck-") . (nil . "flyc-")))

    (which-key-add-key-based-replacements
      "M-s h" "highlight"
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
  :bind ("C-c t r" . writeroom-mode))

;; Trim trailing white-space
(use-package ws-butler
  :diminish (ws-butler-mode . " ☯")
  :commands (ws-butler-global-mode)
  :defer 1
  :config
  (ws-butler-global-mode)
  (with-eval-after-load 'editorconfig
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
                  (ws-butler-mode -1))))))

;; Temporary one window
(use-package zoom-window
  :bind ("C-c w z" . zoom-window-zoom)
  :config (setq zoom-window-mode-line-color "PaleGoldenrod"))

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
  :mode (("\\.ex\\'"      . elixir-mode)
         ("\\.exs\\'"     . elixir-mode)
         ("mix\\.lock\\'" . elixir-mode))
  :config
  (progn
    (use-package alchemist
      :diminish (alchemist-mode)
      :init (add-hook 'elixir-mode-hook #'alchemist-mode))

    (use-package flycheck-credo
      :config
      (with-eval-after-load 'flycheck
        (flycheck-credo-setup)))))

;; Support for Elm
(use-package elm-mode
  :mode ("\\.elm\\'" . elm-mode)
  :commands (elm-mode)
  :preface
  (progn
    (autoload 'company-mode "company")
    (defun my-elm-company-hook ()
      (setq-local company-backends '(company-elm))
      (company-mode)))
  :config
  (progn
    (setq elm-tags-on-save t)
    (setq elm-format-on-save t)

    (add-hook 'elm-mode-hook #'my-elm-company-hook)

    ;; Flycheck support for elm
    (use-package flycheck-elm
      :commands (flycheck-elm-setup)
      :init
      (with-eval-after-load 'flycheck
        (add-hook 'elm-mode-hook #'flycheck-elm-setup)))))

;; EPUB Reader
(use-package ereader
  :mode ("\\.epub$" . ereader-mode)
  :init (add-hook 'ereader-mode-hook #'my-epub-mode-hook)
  :preface
  (defun my-epub-mode-hook ()
    "Setup Epub mode."
    (page-break-lines-mode t)))

;; Support for Erlang
(use-package erlang
  :mode (("\\.erl\\'" . erlang-mode)
         ("\\.hrl\\'" . erlang-mode)
         ("\\.xrl\\'" . erlang-mode))
  :config
  (use-package distel
    :ensure nil
    :load-path "/usr/share/distel/elisp/"
    :commands (erlang-extended-mode)
    :defer t
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
      (defun my-erlang-company-hook ()
        (setq-local company-backends '(company-distel))
        (company-mode)))
    :config
    (add-hook 'erlang-mode-hook #'my-erlang-company-hook)))

;; Support for Fish
(use-package fish-mode
  :mode (("\\.fish\\'"           . fish-mode)
         ("/fish_funced\\..*\\'" . fish-mode))
  :commands (fish-mode)
  :preface
  (defun my-fish-mode-hook ()
    "Setup Fish mode."
    (add-hook 'before-save-hook #'fish_indent-before-save))
  :config (add-hook 'fish-mode-hook #'my-fish-mode-hook))

(use-package gitattributes-mode :defer t)
(use-package gitconfig-mode     :defer t)
(use-package gitignore-mode     :defer t)

;; Support for Go
(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :functions (godef-jump)
  :bind
  (:map
   go-mode-map
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
        (defun my-go-company-hook ()
          (setq-local company-backends '(company-go))
          (company-mode)))
      :config
      (progn
        (setq company-go-show-annotation t)
        (add-hook 'go-mode-hook #'my-go-company-hook)))

    ;; Documentation support
    (use-package go-eldoc
      :after go-mode
      :init (add-hook 'go-mode-hook #'go-eldoc-setup))

    ;; Ask questions about Go source code
    (use-package go-guru
      :bind
      (:map
       go-mode-map
       ("C-c f d" . go-guru-definition)
       ("C-c f r" . go-guru-referrers)
       ("C-c f s" . go-guru-callstack)
       ("C-c f i" . go-guru-implements))
      :init (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode))

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
  (:map
   haskell-mode-map
   ("C-c c c" . haskell-compile)
   ("<f5>"    . haskell-compile))
  :init (remove-hook 'haskell-mode-hook 'interactive-haskell-mode)
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
      :commands (turn-on-hi2 hi2-mode)
      :defer t
      :init (add-hook 'haskell-mode-hook #'turn-on-hi2)
      :config
      (progn
        ;; Unbind RET binding since the shm mode binding is preferred.
        (with-eval-after-load 'hi2
          (define-key hi2-mode-map [?\r] nil))

        (with-eval-after-load 'editorconfig
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
                                         2))))))))

    ;; Interactive development for Haskell
    ;; (completions, type checking, jump to definition, type selection,
    ;; suggestions)
    (use-package intero
      :diminish (intero-mode . " λ")
      :commands (intero-mode)
      :defer t
      :init (add-hook 'haskell-mode-hook #'intero-mode)
      :config (setq haskell-process-args-stack-ghci
                    '("--ghc-options=-ferror-spans" "--with-ghc=intero")))

    ;; Structured editing operations
    (use-package shm
      :commands
      (structured-haskell-mode
       structured-haskell-repl-mode
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
          (setq evil-insert-count count)
          (setq evil-insert-lines t)
          (setq evil-insert-vcount nil)
          (evil-insert-state 1)
          (add-hook 'post-command-hook #'evil-maybe-remove-spaces))

        (defun evil-shm/open-below (count)
          "Insert a new line below point and switch to Insert state.
The insertion will be repeated COUNT times."
          (interactive "p")
          (goto-char (line-end-position))
          (shm/newline-indent)
          (setq evil-insert-count count)
          (setq evil-insert-lines t)
          (setq evil-insert-vcount nil)
          (evil-insert-state 1)
          (add-hook 'post-command-hook #'evil-maybe-remove-spaces)))
      :defer t
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
  :mode ("\\.js\\'" . js2-mode)
  :config
  (progn
    ;; JavaScript code analyzer
    (use-package tern
      :commands (tern-mode)
      :defer t
      :init (add-hook 'js2-mode-hook #'tern-mode))

    ;; Completion for Tern
    (use-package company-tern
      :after tern
      :commands (company-tern)
      :preface
      (progn
        (autoload 'company-mode "company")
        (defun my-js-company-hook ()
          (setq-local company-backends '(company-tern))
          (company-mode)))
      :config (add-hook 'tern-mode-hook #'my-js-company-hook))))

;; Support for JSON
(use-package json-mode
  :mode ("\\.json\\'" . json-mode))

;; Support for Markdown
(use-package markdown-mode
  :mode
  (("\\`README\\.md\\'" . gfm-mode)
   ("\\.md\\'"          . markdown-mode)
   ("\\.markdown\\'"    . markdown-mode))
  :commands (markdown-mode gfm-mode)
  :bind
  (:map
   markdown-mode-map
   ("TAB"             . nil)
   ("<S-iso-lefttab>" . nil)
   ("<S-tab>"         . nil)
   ("<backtab>"       . nil))
  :preface
  (defun my-markdown-mode-hook ()
    "Setup Markdown mode."
    (variable-pitch-mode t))
  :config
  (progn
    (setq markdown-enable-wiki-links t)
    (setq markdown-wiki-link-fontify-missing t)

    (add-hook 'markdown-mode-hook #'my-markdown-mode-hook)
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
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :commands (pdf-view-mode pdf-tools-install))

;; Support for PKGBUILD
(use-package pkgbuild-mode
  :mode ("/PKGBUILD\\'" . pkgbuild-mode))

;; Support for PlantUML
(use-package plantuml-mode
  :mode ("\\.p\\(lant\\)?uml\\'" . plantuml-mode)
  :config
  (progn
    (setq plantuml-java-command "java-headless")
    (setq plantuml-jar-path "/opt/plantuml/plantuml.jar")

    ;; Flycheck support for PlantUML
    (use-package flycheck-plantuml
      :commands (flycheck-plantuml-setup)
      :init
      (with-eval-after-load 'flycheck
        (flycheck-plantuml-setup)))))

;; Support for Protocol Buffers
(use-package protobuf-mode
  :mode ("\\.proto\\'" . protobuf-mode))

;; Support for Puppet
(use-package puppet-mode
  :mode
  (("\\.pp\\'"      . puppet-mode)
   ("Puppetfile\\'" . puppet-mode)))

;; Support for Python
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :preface
  (defun my-python-mode-hook ()
    "Setup Python mode."
    (highlight-indent-guides-mode))
  :config
  (progn
    (add-hook 'python-mode-hook #'my-python-mode-hook)

    ;; Code navigation, documentation lookup and completion
    (use-package anaconda-mode
      :defer t
      :commands (anaconda-mode anaconda-eldoc-mode)
      :init
      (progn
        (add-hook 'python-mode-hook 'anaconda-mode)
        (add-hook 'python-mode-hook 'anaconda-eldoc-mode))
      :config (setq anaconda-mode-installation-directory
                    (concat my-data-directory "anaconda-mode")))

    ;; Completion for anaconda mode
    (use-package company-anaconda
      :after anaconda-mode
      :preface
      (progn
        (autoload 'company-mode "company")
        (defun my-python-company-hook ()
          (setq-local company-backends '(company-anaconda))
          (company-mode)))
      :init (add-hook 'python-mode-hook #'my-python-company-hook))))

;; Support for ReStructured Text
(use-package rst
  :mode
  (("\\.txt\\'"  . rst-mode)
   ("\\.rst\\'"  . rst-mode)
   ("\\.rest\\'" . rst-mode))
  :bind
  (:map
   rst-mode-map
   ("M-RET" . rst-insert-list))
  :preface
  (defun my-rst-mode-hook ()
    "Setup ReStructured Text mode."
    (variable-pitch-mode t))
  :config
  (progn
    (add-hook 'rst-mode-hook #'my-rst-mode-hook)

    ;; Header underline display
    (set-face-attribute 'rst-adornment nil
                        :strike-through "black"
                        :foreground (face-attribute 'default :background)
                        :background (face-attribute 'default :background))

    ;; Typography
    (set-face-attribute 'rst-literal nil :inherit 'fixed-pitch)

    (set-face-attribute 'rst-level-1 nil
                        :background 'unspecified :weight 'bold :height 1.8)
    (set-face-attribute 'rst-level-2 nil
                        :background 'unspecified :weight 'bold :height 1.4)
    (set-face-attribute 'rst-level-3 nil
                        :background 'unspecified :weight 'bold :height 1.2)
    (set-face-attribute 'rst-level-4 nil
                        :background 'unspecified :weight 'bold :height 1.0)
    (set-face-attribute 'rst-level-5 nil
                        :background 'unspecified :weight 'bold :height 1.0)
    (set-face-attribute 'rst-level-6 nil
                        :background 'unspecified :weight 'bold :height 1.0)))

;; Support for Ruby
(use-package enh-ruby-mode
  :mode
  (("\\.rb\\'"       . enh-ruby-mode)
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
    ;; Don't indent the parenthesis or bracket based on the previous line.
    (setq enh-ruby-deep-indent-paren nil)

    ;; Code navigation, documentation lookup and completion
    (use-package robe
      :commands (robe-mode robe-start)
      :preface
      (progn
        (autoload 'company-mode "company")
        (autoload 'company-keywords "company")
        (defun my-ruby-mode-hook ()
          "Setup Ruby mode."
          (robe-mode)

          ;; Setup completion backends
          (setq-local company-backends '((company-keywords
                                          company-files
                                          company-robe)))
          (company-mode)))
      :defer t
      :init (add-hook 'enh-ruby-mode-hook #'my-ruby-mode-hook))

    ;; REPL buffer
    (use-package inf-ruby
      :commands (inf-ruby inf-ruby-auto-enter)
      :defer t
      :config
      (progn
        (setq inf-ruby-default-implementation "pry")
        (add-hook 'enh-ruby-mode-hook #'inf-ruby-minor-mode)
        (add-hook 'compilation-filter-hook #'inf-ruby-auto-enter)))

    ;; Auto-insert end keyword
    (use-package ruby-end
      :commands (ruby-end-mode)
      :config
      (progn
        (setq ruby-end-insert-newline nil)
        (add-hook 'enh-ruby-mode-hook #'ruby-end-mode)))

    ;; Toggle strings and symbols
    (use-package ruby-tools
      :config
      (add-hook 'enh-ruby-mode-hook #'ruby-tools-mode))

    ;; Test runner
    (use-package ruby-test-mode
      :commands (ruby-test-run-at-point ruby-test-run))

    ;; Rubocop integration
    (use-package rubocop
      :diminish (rubocop-mode)
      :commands (rubocop-mode)
      :config (add-hook 'enh-ruby-mode-hook #'rubocop-mode))))

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
      (with-eval-after-load 'flycheck
        (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

    ;; Racer support (completion, definition lookup, describe function/type)
    (use-package racer
      :defines (racer-rust-src-path)
      :commands (racer-mode racer-describe racer-find-definition)
      :defer t
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
      :config (add-hook 'rust-mode-hook #'cargo-minor-mode))))


;; Support for POSIX-based shell scripts
(use-package sh-script
  :mode
  (("\\.sh\\'"   . sh-mode)
   ("\\.zsh\\'"  . sh-mode)
   ("\\.bash\\'" . sh-mode))
  :config
  (progn
    ;; Use regular indentation for line-continuation
    (setq sh-indent-after-continuation 'always)

    ;; Typography
    (set-face-background 'sh-quoted-exec
                         (face-attribute 'font-lock-builtin-face :background))
    (set-face-foreground 'sh-quoted-exec
                         (face-attribute 'font-lock-builtin-face :foreground))))

;; Support for slim templates
(use-package slim-mode
  :mode ("\\.slim\\'" . slim-mode))

;; Support for SQL
(use-package sql
  :mode ("\\.sql\\'" . sql-mode)
  :commands (sql-connect sql-set-product)
  :bind
  (("C-c a s" . sql-connect)
   :map
   sql-mode-map
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
  (progn
    (setq web-mode-enable-current-element-highlight t)

    ;; No padding for nested sections inside HTML
    (with-eval-after-load 'editorconfig
      (add-hook 'editorconfig-custom-hooks
                (lambda (props)
                  (setq web-mode-block-padding 0)
                  (setq web-mode-script-padding 0)
                  (setq web-mode-style-padding 0))))

    ;; Zen Coding support
    (use-package emmet-mode
      :commands (emmet-mode)
      :defer t
      :init (add-hook 'web-mode-hook #'emmet-mode))

    ;; Completion for web mode
    (use-package company-web
      :preface
      (progn
        (autoload 'company-mode "company")
        (defun my-web-company-hook ()
          (setq-local company-backends '(company-web-html))
          (company-mode)))
      :config (add-hook 'web-mode-hook #'my-web-company-hook))))

;; Support for YAML files
(use-package yaml-mode
  :mode ("\\.\\(e?ya?\\|ra\\)ml\\'" . yaml-mode)
  :config
  (progn
    ;; Documentation lookup for Ansible,
    ;; this can be issued by `C-c ?'
    (use-package ansible-doc
      :diminish (ansible-doc-mode)
      :commands (ansible-doc-mode)
      :config (add-hook 'yaml-mode-hook #'ansible-doc-mode))

    ;; Completion for Ansible keywords
    (use-package company-ansible
      :preface
      (progn
        (autoload 'company-mode "company")
        (defun my-yaml-company-hook ()
          (setq-local company-backends '(company-ansible))
          (company-mode)))
      :config (add-hook 'yaml-mode-hook #'my-yaml-company-hook))))

;; Completion for shell functions and executable files in PATH
(use-package company-shell
  :preface
  (progn
    (autoload 'company-mode "company")
    (defun my-sh-company-hook ()
      (setq-local company-backends '((company-shell company-files)))
      (company-mode))
    (defun my-fish-company-hook ()
      (autoload 'company-keywords "company")

      ;; Setup fish keywords
      (when (bound-and-true-p company-keywords-alist)
        (add-to-list 'company-keywords-alist
                     (cons 'fish-mode (append fish-builtins
                                              fish-keywords))))

      ;; Setup backends
      (setq-local company-backends '((company-keywords
                                      company-files
                                      company-shell
                                      company-fish-shell)))
      (company-mode)))
  :config
  (progn
    (setq company-shell-delete-duplicates t)
    (add-hook 'sh-mode-hook #'my-sh-company-hook)
    (add-hook 'fish-mode-hook #'my-fish-company-hook)))

;;; YCMD - Completion, Documentation and Linting
(use-package ycmd
  :if (getenv "YCMD_PATH")
  :defines
  (ycmd-server-command
   ycmd-extra-conf-handler
   ycmd-max-num-identifier-candidates)
  :commands (ycmd-mode ycmd-eldoc-setup)
  :init
  (dolist (hook '(c-mode-hook c++-mode-hook))
    (add-hook hook #'ycmd-mode))
  :config
  (progn
    (setq ycmd-extra-conf-handler 'load)
    (setq ycmd-max-num-identifier-candidates 30)

    (setq ycmd-server-command (list "python2" (getenv "YCMD_PATH")))
    (setq-default ycmd-global-config (concat user-emacs-directory
                                             "ycm_extra_conf.py"))

    (require 'ycmd-eldoc)
    (add-hook 'ycmd-mode-hook #'ycmd-eldoc-setup)))

(use-package company-ycmd
  :commands (company-ycmd-setup)
  :after ycmd
  :defer t
  :init (add-hook 'ycmd-mode-hook #'company-ycmd-setup))

(use-package flycheck-ycmd
  :commands (flycheck-ycmd-setup)
  :after ycmd
  :defer t
  :init
  (with-eval-after-load 'flycheck
    (add-hook 'ycmd-mode-hook #'flycheck-ycmd-setup)))

;;; Modeline
(setq-default mode-line-format
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
