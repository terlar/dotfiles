;;; show-tab-width-mode.el --- show tab width in status bar

;;; Commentary:
;;
;; A simple minor-mode to display the tab-width in the status bar.

;;; Code:

(defgroup show-tab-width-mode nil
  "Display tab width in mode line of Emacs."
  :group 'modeline)

(defcustom show-tab-width-mode-prefix ""
  "Text to display before the tab width in the mode line."
  :type 'string
  :group 'show-tab-width-mode)

(defcustom show-tab-width-mode-suffix ":"
  "Text to display after the tab width in the mode line."
  :type 'string
  :group 'show-tab-width-mode)

(defcustom show-tab-width-mode-fancy t
  "Indicates wether to replace the `tab-width' number or not."
  :group 'show-tab-width-mode)

(defcustom show-tab-width-mode-fancy-alist
  '((0 . "⓪")
    (2 . "②")
    (4 . "④")
    (8 . "⑧"))
  "Alist mapping `tab-width' numbers to the value used in the mode line.

Each element is a list of the form (KEY . VALUE)."
  :group 'show-tab-width-mode)

;;;###autoload
(defvar tab-width-mode
  '((show-tab-width-mode
     (:propertize
      (:eval
       (concat
        show-tab-width-mode-prefix
        (if show-tab-width-mode-fancy
            (cdr (assoc tab-width show-tab-width-mode-fancy-alist))
          (int-to-string tab-width))
        show-tab-width-mode-suffix))
      'help-echo "Tab-width")))
  "String to display `tab-width' in the mode line.")
(put 'tab-width-mode 'risky-local-variable t)

;;;###autoload
(define-minor-mode show-tab-width-mode
  "Toggle show-tab-width mode
With no argument, this command toggles the mode.
A non-null prefix argument turns the mode on.
A null prefix argument turns it off.

When enabled, the value of `tab-width' is displayed in the mode line (at
the very beginning)."
  :global t
  :group 'show-tab-width-mode)

(provide 'show-tab-width-mode)
;;; show-tab-width-mode.el ends here
