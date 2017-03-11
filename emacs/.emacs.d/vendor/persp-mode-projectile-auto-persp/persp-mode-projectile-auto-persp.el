;;; persp-mode-projectile-auto-persp.el --- persp-mode + projectile auto prespectives. -*- lexical-binding: t -*-

;;; Commentary:

;; Auto-creates a perspective for each projectile project file.

;;; Code:

(require 'persp-mode)
(require 'projectile)

(defvar persp-mode-projectile-auto-persp-mode nil)

(defgroup persp-mode-projectile-auto-persp nil
  "persp-mode projectile auto perspectives."
  :group 'persp-mode
  :group 'projectile
  :prefix "persp-mode-projectile-auto-persp-")

;;;###autoload
(define-minor-mode persp-mode-projectile-auto-persp-mode
  :require 'persp-projectile
  :group 'persp-projectile
  :init-value nil
  :global t

  (if persp-mode-projectile-auto-persp-mode
      (if (and persp-mode projectile-mode)
          (progn

            (defvar persp-mode-projectile-bridge-before-switch-selected-window-buffer nil)

            (persp-def-auto-persp "projectile"
                                  :parameters '((dont-save-to-file . t)
                                                (persp-mode-projectile-bridge . t))
                                  :hooks '(projectile-before-switch-project-hook
                                           projectile-after-switch-project-hook
                                           projectile-find-file-hook
                                           find-file-hook)
                                  :dyn-env '((after-switch-to-buffer-adv-suspend t))
                                  :switch 'frame
                                  :predicate
                                  #'(lambda (buffer &optional state)
                                      (if (eq 'projectile-before-switch-project-hook
                                              (alist-get 'hook state))
                                          state
                                        (and
                                         projectile-mode
                                         (buffer-live-p buffer)
                                         (buffer-file-name buffer)
                                         ;; (not git-commit-mode)
                                         (projectile-project-p)
                                         (or state t))))
                                  :get-name
                                  #'(lambda (state)
                                      (if (eq 'projectile-before-switch-project-hook
                                              (alist-get 'hook state))
                                          state
                                        (push (cons 'persp-name
                                                    (concat "p) "
                                                            (with-current-buffer (alist-get 'buffer state)
                                                              (projectile-project-name))))
                                              state)
                                        state))
                                  :on-match
                                  #'(lambda (state)
                                      (let ((hook (alist-get 'hook state))
                                            (persp (alist-get 'persp state))
                                            (buffer (alist-get 'buffer state)))
                                        (case hook
                                          (projectile-before-switch-project-hook
                                           (let ((win (if (minibuffer-window-active-p (selected-window))
                                                          (minibuffer-selected-window)
                                                        (selected-window))))
                                             (when (window-live-p win)
                                               (setq persp-mode-projectile-bridge-before-switch-selected-window-buffer
                                                     (window-buffer win)))))

                                          (projectile-after-switch-project-hook
                                           (when (buffer-live-p
                                                  persp-mode-projectile-bridge-before-switch-selected-window-buffer)
                                             (let ((win (selected-window)))
                                               (unless (eq (window-buffer win)
                                                           persp-mode-projectile-bridge-before-switch-selected-window-buffer)
                                                 (set-window-buffer
                                                  win persp-mode-projectile-bridge-before-switch-selected-window-buffer)))))

                                          (find-file-hook
                                           (setcdr (assq :switch state) nil)))
                                        (if (case hook
                                              (projectile-before-switch-project-hook nil)
                                              (t t))
                                            (persp--auto-persp-default-on-match state)
                                          (setcdr (assq :after-match state) nil)))
                                      state)
                                  :after-match
                                  #'(lambda (state)
                                      (when (eq 'find-file-hook (alist-get 'hook state))
                                        (run-at-time 0.5 nil
                                                     #'(lambda (buf persp)
                                                         (when (and (eq persp (get-current-persp))
                                                                    (not (eq buf (window-buffer (selected-window)))))
                                                           ;; (switch-to-buffer buf)
                                                           (persp-add-buffer buf persp t nil)))
                                                     (alist-get 'buffer state)
                                                     (get-current-persp)))
                                      (persp--auto-persp-default-after-match state))))
        (message "You can not enable persp-mode-projectile-auto-persp-mode \
unless persp-mode and projectile-mode are active.")
        (setq persp-mode-projectile-auto-persp-mode nil))

    (persp-def-auto-persp "projectile" :delete t)))

(provide 'persp-mode-projectile-auto-persp)
;;; persp-mode-projectile-auto-persp.el ends here
