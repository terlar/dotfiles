;;; persp-projectile-bridge.el --- persp-mode + projectile integration.

;; Copyright (C) 2017 Constantin Kulikov
;;
;; Author: Constantin Kulikov (Bad_ptr) <zxnotdead@gmail.com>
;; Version: 0.1
;; Package-Requires: (persp-mode projectile)
;; Date: 2017/03/04 10:10:41
;; License: GPL either version 3 or any later version
;; Keywords: persp-mode, projectile
;; URL: https://gist.github.com/Bad-ptr/1aca1ec54c3bdb2ee80996eb2b68ad2d#file-persp-projectile-bridge-el

;;; License:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Create a perspective for each projectile project.

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'persp-projectile-bridge)

;; (with-eval-after-load "persp-projectile-bridge"
;;   (add-hook 'persp-projectile-bridge-mode-hook
;;             #'(lambda ()
;;                 (if persp-projectile-bridge-mode
;;                     (persp-projectile-bridge-find-perspectives-for-all-buffers)
;;                   (persp-projectile-bridge-kill-perspectives)))))
;; (persp-projectile-bridge-mode 1)

;;; Code:


(require 'persp-mode)
(require 'projectile)

(defun persp-projectile-bridge-add-new-persp (name)
  (let ((persp (persp-get-by-name name *persp-hash* :nil)))
    (if (eq :nil persp)
        (prog1
            (setq persp (persp-add-new name))
          (when persp
            (set-persp-parameter 'persp-projectile-bridge t persp)
            (set-persp-parameter 'dont-save-to-file t persp)
            (persp-add-buffer (projectile-project-buffers)
                              persp nil nil)))
      persp)))

(defun persp-projectile-bridge-find-perspective-for-buffer (b)
  (with-current-buffer b
    (when (and persp-mode projectile-mode persp-projectile-bridge-mode
               (buffer-file-name b)
               (projectile-project-p))
      (let ((persp (persp-projectile-bridge-add-new-persp
                    (projectile-project-name))))
        (when persp
          (persp-add-buffer b persp nil nil)
          persp)))))

(defvar persp-projectile-bridge-before-switch-selected-window-buffer nil)
(defun persp-projectile-bridge-hook-before-switch (&rest arts)
  (let ((win (if (minibuffer-window-active-p (selected-window))
                 (minibuffer-selected-window)
               (selected-window))))
    (when (window-live-p win)
      (setq persp-projectile-bridge-before-switch-selected-window-buffer
            (window-buffer win)))))

(defun persp-projectile-bridge-hook-switch (&rest args)
  (let ((persp
         (persp-projectile-bridge-find-perspective-for-buffer
          (current-buffer))))
    (when persp
      (when (buffer-live-p
             persp-projectile-bridge-before-switch-selected-window-buffer)
        (let ((win (selected-window)))
          (unless (eq (window-buffer win)
                      persp-projectile-bridge-before-switch-selected-window-buffer)
            (set-window-buffer
             win persp-projectile-bridge-before-switch-selected-window-buffer))))
      (persp-frame-switch (persp-name persp)))))

(defun persp-projectile-bridge-hook-find-file (&rest args)
  (let ((persp
         (persp-projectile-bridge-find-perspective-for-buffer
          (current-buffer))))
    (when persp
      (persp-add-buffer (current-buffer) (persp-name persp)))))

(defun persp-projectile-bridge-find-perspectives-for-all-buffers ()
  (when (and persp-mode projectile-mode persp-projectile-bridge-mode)
    (mapc #'persp-projectile-bridge-find-perspective-for-buffer (buffer-list))))

(defun persp-projectile-bridge-kill-perspectives ()
  (when (and persp-mode projectile-mode)
    (mapc #'persp-kill (mapcar #'persp-name
                               (delete-if-not
                                (apply-partially #'persp-parameter 'persp-projectile-bridge)
                                (persp-persps))))))

;;;###autoload
(define-minor-mode persp-projectile-bridge-mode
  :require 'persp-projectile
  :group 'persp-projectile
  :init-value nil
  :global t

  (if persp-projectile-bridge-mode
      (if (and persp-mode projectile-mode)
          (progn
            (add-hook 'projectile-mode-hook
                      #'(lambda () (unless projectile-mode
                                (persp-projectile-bridge-mode -1))))
            (add-hook 'persp-mode-hook
                      #'(lambda () (unless persp-mode
                                (persp-projectile-bridge-mode -1))))
            (add-hook 'projectile-before-switch-project-hook
                      #'persp-projectile-bridge-hook-before-switch)
            (add-hook 'projectile-after-switch-project-hook
                      #'persp-projectile-bridge-hook-switch)
            (add-hook 'projectile-find-file-hook
                      #'persp-projectile-bridge-hook-switch))
        (message "You can not enable persp-projectile-bridge-mode \
unless persp-mode and projectile-mode are active.")
        (setq persp-projectile-bridge-mode nil))
    (remove-hook 'projectile-before-switch-project-hook
                 #'persp-projectile-bridge-hook-before-switch)
    (remove-hook 'projectile-after-switch-project-hook
                 #'persp-projectile-bridge-hook-switch)
    (remove-hook 'projectile-find-file-hook
                 #'persp-projectile-bridge-hook-switch)))


(provide 'persp-projectile-bridge)

;;; persp-projectile-bridge.el ends here
