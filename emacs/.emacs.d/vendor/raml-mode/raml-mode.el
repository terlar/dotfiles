;;; raml-mode.el --- Major mode for editing RAML (RESTful API Modeling
;;; Lanuguage) files

;; Copyright (c) 2016 Victor Quinn

;; Author: Victor Quinn <mail@victorquinn.com>
;; URL: https://github.com/victorquinn/raml-mode
;; Keywords: raml languages
;; Version 0.0.1

;; This file is distributed under the terms of the MIT license so feel free to
;; hack away :)

;;; Commentary:

;; This is a major mode for editing RAML files. RAML stands for RESTful API
;; Modeling Language. More info can be found on http://raml.org

;; Note, the author of this mode is not officially affiliated with RAML but
;; thought RAML was great and found there was no major mode for Emacs.

;; This mode is developed for RAML 1.0 and no RAML 0.8 support is here or
;; likely to be developed.

(defconst raml-mode-version "0.0.1" "Version of `raml-mode`.")

(defvar raml-mode-hook nil)
(add-to-list 'auto-mode-alist '("\\.raml\\'" . raml-mode))

(defconst raml-font-lock-keywords
  (list
   '("\\(#%RAML 1\\.0\\)" . font-lock-comment-face)
   '("\\(\\(?:baseUri\\|content\\|documentation\\|mediaType\\|protocols\\|resourceTypes\\|secur\\(?:edBy\\|itySchemes\\)\\|t\\(?:itle\\|\\(?:rait\\|ype\\)s\\)\\|version\\):\\)" . font-lock-variable-name-face))
  "Minimal highlighting expressions for RAML mode")

(defun raml-indent-line ()
  "Indent current line as RAML"
  (interactive)
  (beginning-of-line)
  (if (bobp) (indent-line-to 0))
  ;; ...
  )

(defun raml-mode-version()
  "Display version of `raml-mode`."
  (interactive)
  (message "raml-mode %s" raml-mode-version)
  raml-mode-version)

(defun raml-mode ()
  "Major mode for editing RAML (RESTful API Modeling Language) files"
  (interactive)
  (kill-all-local-variables)
  (set (make-local-variable 'font-lock-defaults) '(raml-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'raml-indent-line)
  (setq major-mode 'raml-mode)
  (setq mode-name "RAML")
  (run-hooks 'raml-mode-hook))

(provide 'raml-mode)

;;; raml-mode.el ends here
